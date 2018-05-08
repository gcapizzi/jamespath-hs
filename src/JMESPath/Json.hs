module JMESPath.Json
  ( Value
  -- decode and encode
  , decode
  , decodeString
  , encode
  -- getters
  , lookupKey
  , lookupIndex
  , slice
  -- high order functions
  , mapArray
  , mapObject
  , flatMapArray
  , filterArray
  -- constructors
  , null
  , array
  , object
  , bool
  , string
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
import Prelude hiding (null)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Vector as Vector

newtype Value = Value Aeson.Value deriving (Show, Eq)

-- decode and encode

decode :: ByteString -> Either String Value
decode document = Value <$> (Aeson.eitherDecode document :: Either String Aeson.Value)

decodeString :: String -> Either String Value
decodeString = decode . ByteString.pack

encode :: Value -> ByteString
encode (Value value) = Aeson.encode value

-- getters

lookupKey :: Text -> Value -> Value
lookupKey key (Value (Aeson.Object values)) = Value $ HashMap.lookupDefault Aeson.Null key values
lookupKey _ _ = null

lookupIndex :: Int -> Value -> Value
lookupIndex index (Value (Aeson.Array values)) = Value $ Maybe.fromMaybe Aeson.Null $ values Vector.!? normalizedIndex
  where
    normalizedIndex = if index < 0 then Vector.length values + index else index
lookupIndex _ _ = null

slice :: Maybe Int -> Maybe Int -> Maybe Int -> Value -> Either String Value
slice _ _ (Just 0) _ = Left "Error: slice step cannot be 0"
slice maybeFrom maybeTo maybeStep (Value (Aeson.Array values))
    | step > 0 = let count = if from < to then to - from else 0
                     subList = Vector.slice from count values
                 in Right $ Value $ Aeson.Array $ eachEvery step subList
    | step < 0 = let count = if to < from then from - to else 0
                     subList = Vector.slice (len - from - 1) count (Vector.reverse values)
                 in Right $ Value $ Aeson.Array $ eachEvery (-step) subList
  where
    from = Maybe.maybe (if step < 0 then len - 1 else 0) (capSlice step len) maybeFrom
    to = Maybe.maybe (if step < 0 then -1 else len) (capSlice step len) maybeTo
    step = Maybe.fromMaybe 1 maybeStep
    len = Vector.length values
slice _ _ _ _ = Right null

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
mapArray f (Value (Aeson.Array values)) = Value <$> mapValues (fmap toAeson . f . fromAeson) values
mapArray _ _ = return null

mapObject :: Monad m => (Value -> m Value) -> Value -> m Value
mapObject f (Value (Aeson.Object values)) = Value <$> mapValues (fmap toAeson . f . fromAeson) elems
  where
    elems = Vector.fromList $ HashMap.elems values
mapObject _ _ = return null

mapValues :: Monad m => (Aeson.Value -> m Aeson.Value) -> Vector Aeson.Value -> m Aeson.Value
mapValues f values = do
    resultWithNulls <- Vector.mapM f values
    let result = Vector.filter (/= Aeson.Null) resultWithNulls
    return $ Aeson.Array result

flatMapArray :: Monad m => (Value -> m Value) -> Value -> m Value
flatMapArray f value = mapArray f $ flattenArray value

flattenArray :: Value -> Value
flattenArray (Value (Aeson.Array values)) = Value $ Aeson.Array $ Vector.foldr f Vector.empty values
  where
    f (Aeson.Array subarray) acc = Vector.filter (/= Aeson.Null) subarray Vector.++ acc
    f Aeson.Null acc = acc
    f v acc = Vector.cons v acc
flattenArray _ = null

filterArray :: Monad m => (Value -> m Value) -> Value -> m Value
filterArray f (Value (Aeson.Array values)) = Value <$> filterValues (fmap toAeson . f . fromAeson) values
filterArray _ _ = return null

filterValues :: Monad m => (Aeson.Value -> m Aeson.Value) -> Vector Aeson.Value -> m Aeson.Value
filterValues f values = Aeson.Array <$> Vector.filterM (fmap isAesonTruthy . f) values

-- constructors

null :: Value
null = Value Aeson.Null

bool :: Bool -> Value
bool = Value . Aeson.Bool

array :: [Value] -> Value
array values = Value $ Aeson.Array $ Vector.fromList $ map toAeson values

object :: [(Text, Value)] -> Value
object pairs = Value $ Aeson.Object $ HashMap.fromList $ map (fmap toAeson) pairs

string :: String -> Value
string = Value . Aeson.String . Text.pack

-- predicates

isNull :: Value -> Bool
isNull (Value Aeson.Null) = True
isNull _ = False

isFalsy :: Value -> Bool
isFalsy (Value aesonValue) = isAesonFalsy aesonValue

isTruthy :: Value -> Bool
isTruthy = not . isFalsy

isAesonFalsy :: Aeson.Value -> Bool
isAesonFalsy Aeson.Null = True
isAesonFalsy (Aeson.Array value) = Vector.null value
isAesonFalsy (Aeson.Object value) = HashMap.null value
isAesonFalsy (Aeson.String value) = Text.null value
isAesonFalsy (Aeson.Bool value) = not value
isAesonFalsy _ = False

isAesonTruthy :: Aeson.Value -> Bool
isAesonTruthy = not . isAesonFalsy

-- comparators

equal :: Value -> Value -> Value
equal left right = bool $ left == right

notEqual :: Value -> Value -> Value
notEqual left right = bool $ left /= right

lessThan :: Value -> Value -> Value
lessThan (Value (Aeson.Number left)) (Value (Aeson.Number right)) = bool $ left < right
lessThan _ _ = null

greaterThan :: Value -> Value -> Value
greaterThan (Value (Aeson.Number left)) (Value (Aeson.Number right)) = bool $ left > right
greaterThan _ _ = null

lessThanOrEqual :: Value -> Value -> Value
lessThanOrEqual (Value (Aeson.Number left)) (Value (Aeson.Number right)) = bool $ left <= right
lessThanOrEqual _ _ = null

greaterThanOrEqual :: Value -> Value -> Value
greaterThanOrEqual (Value (Aeson.Number left)) (Value (Aeson.Number right)) = bool $ left >= right
greaterThanOrEqual _ _ = null

-- to/from Aeson

toAeson :: Value -> Aeson.Value
toAeson (Value v) = v

fromAeson :: Aeson.Value -> Value
fromAeson = Value

