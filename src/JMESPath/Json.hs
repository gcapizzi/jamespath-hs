module JMESPath.Json
  ( Value
  -- decode and encode
  , decode
  , encode
  -- getters
  , lookupKey
  , lookupIndex
  , slice
  -- high order functions
  , mapArray
  , mapObject
  , flatMap
  , filterValue
  -- constructors
  , nullValue
  , array
  , object
  , bool
  -- predicates
  , isNull
  , isFalsy
  , isTruthy
  -- comparators
  , equal
  , notEqual
  , lessThan
  , greaterThan
  , lessThanOrEqual
  , greaterThanOrEqual
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Vector as Vector

newtype Value = Value Aeson.Value deriving (Show, Eq)

-- decode and encode

decode :: ByteString -> Either String Value
decode document = Value <$> (Aeson.eitherDecode document :: Either String Aeson.Value)

encode :: Value -> ByteString
encode (Value value) = Aeson.encode value

-- getters

lookupKey :: Text -> Value -> Value
lookupKey key (Value (Aeson.Object object)) = Value $ HashMap.lookupDefault Aeson.Null key object
lookupKey _ _ = nullValue

lookupIndex :: Int -> Value -> Value
lookupIndex index (Value (Aeson.Array array)) = Value $ Maybe.fromMaybe Aeson.Null $ array Vector.!? normalizedIndex
  where
    normalizedIndex = if index < 0 then Vector.length array + index else index
lookupIndex _ _ = nullValue

slice :: Maybe Int -> Maybe Int -> Maybe Int -> Value -> Either String Value
slice _ _ (Just 0) _ = Left "Error: slice step cannot be 0"
slice maybeFrom maybeTo maybeStep (Value (Aeson.Array array))
    | step > 0 = let count = if from < to then to - from else 0
                     subList = Vector.slice from count array
                 in Right $ Value $ Aeson.Array $ eachEvery step subList
    | step < 0 = let count = if to < from then from - to else 0
                     subList = Vector.slice (len - from - 1) count (Vector.reverse array)
                 in Right $ Value $ Aeson.Array $ eachEvery (-step) subList
  where
    from = Maybe.maybe (if step < 0 then len - 1 else 0) (capSlice step len) maybeFrom
    to = Maybe.maybe (if step < 0 then -1 else len) (capSlice step len) maybeTo
    step = Maybe.fromMaybe 1 maybeStep
    len = Vector.length array
slice _ _ _ _ = Right nullValue

capSlice :: Int -> Int -> Int -> Int
capSlice step len actual
    | actual < 0 =
        if actual + len < 0
            then if step < 0
                then -1
                else 0
            else actual + len
    | actual >= len =
        if step < 0
            then len - 1
            else len
    | otherwise = actual

eachEvery :: Int -> Vector a -> Vector a
eachEvery 1 xs = xs
eachEvery step xs
    | Vector.null xs = Vector.empty
    | otherwise = Vector.cons x $ eachEvery step rst
  where
    x = Vector.head xs
    rst = Vector.drop step xs

-- high order functions

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

filterValue :: Monad m => (Value -> m Value) -> Value -> m Value
filterValue f (Value (Aeson.Array array)) = Value <$> filterValues (fmap toAeson . f . fromAeson) array
filterValue _ _ = return nullValue

filterValues :: Monad m => (Aeson.Value -> m Aeson.Value) -> Vector Aeson.Value -> m Aeson.Value
filterValues f values = Aeson.Array <$> Vector.filterM (fmap aesonTruthy . f) values

-- constructors

nullValue :: Value
nullValue = Value Aeson.Null

bool :: Bool -> Value
bool = Value . Aeson.Bool

array :: [Value] -> Value
array values = Value $ Aeson.Array $ Vector.fromList $ map toAeson values

object :: [(Text, Value)] -> Value
object pairs = Value $ Aeson.Object $ HashMap.fromList $ map (fmap toAeson) pairs

-- predicates

isNull :: Value -> Bool
isNull (Value Aeson.Null) = True
isNull _ = False

isFalsy :: Value -> Bool
isFalsy (Value aesonValue) = aesonFalsy aesonValue

isTruthy :: Value -> Bool
isTruthy = not . isFalsy

aesonFalsy :: Aeson.Value -> Bool
aesonFalsy Aeson.Null = True
aesonFalsy (Aeson.Array array) = Vector.null array
aesonFalsy (Aeson.Object object) = HashMap.null object
aesonFalsy (Aeson.String string) = Text.null string
aesonFalsy (Aeson.Bool bool) = not bool
aesonFalsy _ = False

aesonTruthy :: Aeson.Value -> Bool
aesonTruthy = not . aesonFalsy

-- comparators

equal :: Value -> Value -> Value
equal left right = bool $ left == right

notEqual :: Value -> Value -> Value
notEqual left right = bool $ left /= right

lessThan :: Value -> Value -> Value
lessThan (Value (Aeson.Number left)) (Value (Aeson.Number right)) = bool $ left < right
lessThan _ _ = nullValue

greaterThan :: Value -> Value -> Value
greaterThan (Value (Aeson.Number left)) (Value (Aeson.Number right)) = bool $ left > right
greaterThan _ _ = nullValue

lessThanOrEqual :: Value -> Value -> Value
lessThanOrEqual (Value (Aeson.Number left)) (Value (Aeson.Number right)) = bool $ left <= right
lessThanOrEqual _ _ = nullValue

greaterThanOrEqual :: Value -> Value -> Value
greaterThanOrEqual (Value (Aeson.Number left)) (Value (Aeson.Number right)) = bool $ left >= right
greaterThanOrEqual _ _ = nullValue

-- to/from Aeson

toAeson :: Value -> Aeson.Value
toAeson (Value v) = v

fromAeson :: Aeson.Value -> Value
fromAeson = Value

