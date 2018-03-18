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
searchValue (ProjectExpression Root expression) document = Json.mapValue (searchValue expression) document
searchValue (ProjectExpression left right) document = do
    value <- searchValue left document
    Json.mapValue (searchValue right) value
searchValue _ _ = Right Json.nullValue
