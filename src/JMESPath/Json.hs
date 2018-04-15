module JMESPath.Json
  ( Value
  , decode
  , encode
  , getKey
  , getIndex
  , slice
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

slice :: Int -> Int -> Int -> Value -> Either String Value
slice _ _ 0 _ = Left "Error: slice step cannot be 0"
slice from to step (Value (Aeson.Array array))
    | step > 0 = let normalizedFrom = if from < 0 then max 0 (len + from) else min from len
                     normalizedTo = if to < 0 then max 0 (len + to) else min to len
                     count = if normalizedFrom < normalizedTo then normalizedTo - normalizedFrom else 0
                     subList = Vector.slice normalizedFrom count array
                 in Right $ Value $ Aeson.Array $ eachEvery step subList
    | step < 0 = let normalizedFrom = if from < 0 then max (-1) (len + from) else min from (len - 1)
                     normalizedTo = if to < 0 then max (-1) (len + to) else min to (len - 1)
                     count = if normalizedTo < normalizedFrom then normalizedFrom - normalizedTo else 0
                     subList = Vector.slice (len - normalizedFrom - 1) count (Vector.reverse array)
                 in Right $ Value $ Aeson.Array $ eachEvery (-step) subList
  where
    len = Vector.length array
slice _ _ _ _ = Right nullValue

eachEvery :: Int -> Vector a -> Vector a
eachEvery 1 xs = xs
eachEvery step xs
    | Vector.null xs = Vector.empty
    | otherwise = Vector.cons x $ eachEvery step rst
  where
    x = Vector.head xs
    rst = Vector.drop step xs

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
