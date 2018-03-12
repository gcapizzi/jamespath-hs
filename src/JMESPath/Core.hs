module JMESPath.Core
    ( searchValue
    , Expression (..)
    ) where

import qualified JMESPath.Json as Json

import Data.Text (Text)

data Expression = Root
                | Identifier Text
                | SubExpression Expression Expression
                | IndexExpression Expression Int
    deriving Show

searchValue :: Expression -> Json.Value -> Either String Json.Value
searchValue (SubExpression Root (Identifier identifier)) document = Right $ Json.getKey identifier document
searchValue (SubExpression expression (Identifier identifier)) document = do
    leftValue <- searchValue expression document
    Right $ Json.getKey identifier leftValue
searchValue (IndexExpression Root index) document = Right $ Json.getIndex index document
searchValue (IndexExpression expression index) document = do
    leftValue <- searchValue expression document
    Right $ Json.getIndex index leftValue
searchValue _ _ = Right Json.nullValue
