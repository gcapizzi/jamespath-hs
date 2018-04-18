module JMESPath.Core
    ( searchValue
    , Expression (..)
    ) where

import JMESPath.Ast
import qualified JMESPath.Json as Json

searchValue :: Expression -> Json.Value -> Either String Json.Value
searchValue Root document = Right document
searchValue (KeyExpression key Root) document = Right $ Json.getKey key document
searchValue (KeyExpression key expression) document = do
    value <- searchValue expression document
    Right $ Json.getKey key value
searchValue (IndexExpression index Root) document = Right $ Json.getIndex index document
searchValue (IndexExpression index expression) document = do
    value <- searchValue expression document
    Right $ Json.getIndex index value
searchValue (ArrayProjectExpression Root expression) document = Json.mapArray (searchValue expression) document
searchValue (ArrayProjectExpression left right) document = do
    value <- searchValue left document
    Json.mapArray (searchValue right) value
searchValue (ObjectProjectExpression Root expression) document = Json.mapObject (searchValue expression) document
searchValue (ObjectProjectExpression left right) document = do
    value <- searchValue left document
    Json.mapObject (searchValue right) value
searchValue (FlattenExpression Root expression) document = Json.flatMap (searchValue expression) document
searchValue (FlattenExpression left right) document = do
    value <- searchValue left document
    Json.flatMap (searchValue right) value
searchValue (SliceExpression from to step Root expression) document = do
    slice <- Json.slice from to step document
    Json.mapArray (searchValue expression) slice
searchValue (SliceExpression from to step left right) document = do
    value <- searchValue left document
    slice <- Json.slice from to step value
    Json.mapArray (searchValue right) slice
searchValue (PipeExpression left right) document = do
    value <- searchValue left document
    searchValue right value
searchValue _ _ = Right Json.nullValue
