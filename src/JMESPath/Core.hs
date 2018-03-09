module JMESPath.Core
    ( searchValue
    , Expression (..)
    ) where

import Data.Aeson
import Data.HashMap.Strict
import Data.Maybe
import Data.Text (Text)
import qualified Data.Vector as V

data Expression = Root
                | Identifier Text
                | SubExpression Expression Expression
                | IndexExpression Expression Int
    deriving Show

searchValue :: Expression -> Value -> Either String Value
searchValue (SubExpression Root (Identifier identifier)) document = Right $ getKey identifier document
searchValue (SubExpression expression (Identifier identifier)) document = do
    leftValue <- searchValue expression document
    Right $ getKey identifier leftValue
searchValue (IndexExpression Root index) document = Right $ getIndex index document
searchValue (IndexExpression expression index) document = do
    leftValue <- searchValue expression document
    Right $ getIndex index leftValue
searchValue _ _ = Right Null

getKey :: Text -> Value -> Value
getKey key (Object object) = lookupDefault Null key object
getKey _ _ = Null

getIndex :: Int -> Value -> Value
getIndex index (Array array) = fromMaybe Null $ array V.!? normalizedIndex
  where
    normalizedIndex = if index < 0 then V.length array + index else index
getIndex _ _ = Null
