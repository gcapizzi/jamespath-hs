module JMESPath.Core
    ( searchValue
    , Expression (..)
    ) where

import JMESPath.Ast
import qualified JMESPath.Json as Json

searchValue :: Expression -> Json.Value -> Either String Json.Value
searchValue Root document = Right document
searchValue (SubExpression (Identifier identifier) Root) document = Right $ Json.getKey identifier document
searchValue (SubExpression (Identifier identifier) expression) document = do
    value <- searchValue expression document
    Right $ Json.getKey identifier value
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
searchValue _ _ = Right Json.nullValue
