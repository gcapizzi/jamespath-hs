module JMESPath.Core
    ( searchValue
    , Expression (..)
    ) where

import JMESPath.Ast
import qualified JMESPath.Json as Json

searchValue :: Expression -> Json.Value -> Json.Value
searchValue Root document = document
searchValue (SubExpression (Identifier identifier) Root) document = Json.getKey identifier document
searchValue (SubExpression (Identifier identifier) expression) document = Json.getKey identifier (searchValue expression document)
searchValue (IndexExpression index Root) document = Json.getIndex index document
searchValue (IndexExpression index expression) document = Json.getIndex index (searchValue expression document)
searchValue (ProjectExpression Root expression) document = Json.mapValue (searchValue expression) document
searchValue (ProjectExpression left right) document = Json.mapValue (searchValue right) (searchValue left document)
searchValue _ _ = Json.nullValue
