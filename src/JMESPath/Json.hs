module JMESPath.Json
  ( Value
  , decode
  , encode
  , getKey
  , getIndex
  , mapArray
  , mapObject
  , flatMap
  , nullValue
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Maybe
import qualified Data.Vector as Vector

newtype Value = Value Aeson.Value deriving Show

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

mapArray :: Monad m => (Value -> m Value) -> Value -> m Value
mapArray f (Value (Aeson.Array array)) = Value <$> mapValues (fmap toAeson . f . fromAeson) array
mapArray _ _ = return nullValue

mapObject :: Monad m => (Value -> m Value) -> Value -> m Value
mapObject f (Value (Aeson.Object object)) = Value <$> mapValues (fmap toAeson . f . fromAeson) elems
  where
    elems = Vector.fromList $ HashMap.elems object
mapObject _ _ = return nullValue

mapValues :: Monad m => (Aeson.Value -> m Aeson.Value) -> Vector Aeson.Value -> m Aeson.Value
mapValues f values = do
    resultWithNulls <- Vector.mapM f values
    let result = Vector.filter (/= Aeson.Null) resultWithNulls
    return $ Aeson.Array result

flatMap :: Monad m => (Value -> m Value) -> Value -> m Value
flatMap f value = mapArray f $ flattenArray value

flattenArray :: Value -> Value
flattenArray (Value (Aeson.Array array)) = Value $ Aeson.Array $ Vector.foldr f Vector.empty array
  where
    f (Aeson.Array subarray) acc = Vector.filter (/= Aeson.Null) subarray Vector.++ acc
    f Aeson.Null acc = acc
    f v acc = Vector.cons v acc
flattenArray _ = nullValue

fromAeson :: Aeson.Value -> Value
fromAeson = Value

toAeson :: Value -> Aeson.Value
toAeson (Value v) = v

nullValue :: Value
nullValue = Value Aeson.Null
