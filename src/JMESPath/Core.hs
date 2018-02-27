module JMESPath.Core
    ( searchValue
    , Expression (..)
    ) where

import Data.Aeson
import Data.HashMap.Strict
import Data.Text (Text)

data Expression = Identifier Text
                | SubExpression Expression Expression

searchValue :: Expression -> Value -> Either String Value
searchValue (Identifier identifier) (Object object) = Right $ lookupDefault Null identifier object
searchValue (SubExpression left right) document = searchValue left document >>= searchValue right
searchValue _ _ = Right Null
