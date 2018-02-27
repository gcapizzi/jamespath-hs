module JMESPath.Core
    ( searchValue
    , Expression (..)
    ) where

import Data.Aeson
import Data.HashMap.Strict
import Data.Maybe
import Data.Vector
import Data.Text (Text)

data Expression = Identifier Text
                | SubExpression Expression Expression
                | IndexExpression Int

searchValue :: Expression -> Value -> Either String Value
searchValue (Identifier identifier) (Object object) = Right $ lookupDefault Null identifier object
searchValue (SubExpression left right) document = searchValue left document >>= searchValue right
searchValue (IndexExpression n) (Array array) = Right $ fromMaybe Null $ array !? n
searchValue _ _ = Right Null
