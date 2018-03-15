module JMESPath.Core
    ( searchValue
    , Expression (..)
    ) where

import qualified JMESPath.Json as Json

import Data.Text (Text)

data Expression = Root
                | Identifier Text
                | SubExpression Expression Expression
                | IndexExpression Int Expression
    deriving Show

searchValue :: Expression -> Json.Value -> Json.Value
searchValue (SubExpression (Identifier identifier) Root) document = Json.getKey identifier document
searchValue (SubExpression (Identifier identifier) expression) document = Json.getKey identifier (searchValue expression document)
searchValue (IndexExpression index Root) document = Json.getIndex index document
searchValue (IndexExpression index expression) document = Json.getIndex index (searchValue expression document)
searchValue _ _ = Json.nullValue
