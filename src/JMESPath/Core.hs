module JMESPath.Core
    ( searchValue
    , Expression (..)
    ) where

import Data.Aeson
import Data.HashMap.Strict
import Data.Maybe
import Data.Text (Text)
import qualified Data.Vector as V

data Expression = Identifier Text
                | SubExpression Expression Expression
                | IndexExpression Int

searchValue :: Expression -> Value -> Either String Value
searchValue (Identifier identifier) (Object object) = Right $ lookupDefault Null identifier object
searchValue (SubExpression left right) document = searchValue left document >>= searchValue right
searchValue (IndexExpression index) (Array array) = Right $ fromMaybe Null $ array V.!? normalizedIndex
  where
    normalizedIndex = if index < 0 then V.length array + index else index
searchValue _ _ = Right Null
