module JMESPath.Json
  ( Value
  , decode
  , encode
  , getKey
  , getIndex
  , mapValue
  , nullValue
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Maybe
import qualified Data.Vector as Vector

newtype Value = Value Aeson.Value

decode :: ByteString -> Either String Value
decode document = Value <$> (Aeson.eitherDecode document :: Either String Aeson.Value)

encode :: Value -> ByteString
encode (Value value) = Aeson.encode value

getKey :: Text -> Value -> Value
getKey key (Value value) = Value $ lookupOrNull key value

lookupOrNull :: Text -> Aeson.Value -> Aeson.Value
lookupOrNull key (Aeson.Object object) = HashMap.lookupDefault Aeson.Null key object
lookupOrNull _ _ = Aeson.Null

getIndex :: Int -> Value -> Value
getIndex index (Value (Aeson.Array array)) = Value $ Maybe.fromMaybe Aeson.Null $ array Vector.!? normalizedIndex
  where
    normalizedIndex = if index < 0 then Vector.length array + index else index
getIndex _ _ = nullValue

mapValue :: (Value -> Value) -> Value -> Value
mapValue f (Value (Aeson.Array array)) = Value $ Aeson.Array result
  where
    result = Vector.filter (/= Aeson.Null) resultWithNulls
    resultWithNulls = Vector.map (toAeson . f . fromAeson) array
mapValue _ _ = nullValue

fromAeson :: Aeson.Value -> Value
fromAeson = Value

toAeson :: Value -> Aeson.Value
toAeson (Value v) = v

nullValue :: Value
nullValue = Value Aeson.Null
