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

searchValue :: Expression -> Json.Value -> Either String Json.Value
searchValue (SubExpression (Identifier identifier) Root) document = Right $ Json.getKey identifier document
searchValue (SubExpression (Identifier identifier) expression) document = do
    leftValue <- searchValue expression document
    Right $ Json.getKey identifier leftValue
searchValue (IndexExpression index Root) document = Right $ Json.getIndex index document
searchValue (IndexExpression index expression) document = do
    leftValue <- searchValue expression document
    Right $ Json.getIndex index leftValue
searchValue _ _ = Right Json.nullValue
