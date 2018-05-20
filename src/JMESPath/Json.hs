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
  , expression
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
  -- numeric functions
  , abs
  , avg
  , ceil
  , floor
  -- string functions
  , endsWith
  , join
  -- array functions
  , mapExpression
  , max
  -- other functions
  , contains
  , keys
  , length
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Foldable (foldrM)
import Data.Text (Text)
import Data.Vector (Vector)
import Prelude hiding (abs, floor, head, length, max, null, sum, tail)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Maybe
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Prelude (abs, floor, max)

data Value = Value Aeson.Value
           | Expression (Value -> Either String Value)

instance Eq Value where
    (==) (Value left) (Value right) = left == right
    (==) _ _ = False

-- decode and encode

decode :: ByteString -> Either String Value
decode document = Value <$> (Aeson.eitherDecode document :: Either String Aeson.Value)

decodeString :: String -> Either String Value
decodeString = decode . ByteString.pack

encode :: Value -> ByteString
encode (Value value) = Aeson.encode value
encode _ = encode null

encodeString :: Value -> String
encodeString = ByteString.unpack . encode

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
mapArray f (Value (Aeson.Array values)) = Value <$> mapValues (aesonFn f) values
mapArray _ _ = return null

mapObject :: Monad m => (Value -> m Value) -> Value -> m Value
mapObject f (Value (Aeson.Object values)) = Value <$> mapValues (aesonFn f) elems
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
filterArray f (Value (Aeson.Array values)) = Value <$> filterValues (aesonFn f) values
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

expression :: (Value -> Either String Value) -> Value
expression = Expression

-- predicates

isNull :: Value -> Bool
isNull (Value Aeson.Null) = True
isNull _ = False

isFalsy :: Value -> Bool
isFalsy (Value aesonValue) = isAesonFalsy aesonValue
isFalsy _ = True

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

-- numeric functions

abs :: Value -> Either String Value
abs (Value (Aeson.Number n)) = Right $ Value $ Aeson.Number $ Prelude.abs n
abs wrong = invalidTypeOfArgument "abs" wrong

avg :: Value -> Either String Value
avg (Value (Aeson.Array ns)) = if len == zero
    then Right null
    else do
        (Aeson.Number sum) <- foldrM addAeson (Aeson.Number zero) ns
        Right $ Value $ Aeson.Number (sum / len)
  where
    len = Scientific.scientific (fromIntegral $ Vector.length ns) 0
    zero = Scientific.scientific 0 0
avg wrong = invalidTypeOfArgument "avg" wrong

addAeson :: Aeson.Value -> Aeson.Value -> Either String Aeson.Value
addAeson (Aeson.Number left) (Aeson.Number right) = Right $ Aeson.Number (left + right)
addAeson left (Aeson.Number _) = Left $ "avg: invalid type of value '" ++ ByteString.unpack (Aeson.encode left) ++ "'"
addAeson _ _ = Left "avg: invalid type of values"

ceil :: Value -> Either String Value
ceil (Value (Aeson.Number n)) = Right $ Value $ Aeson.Number $ fromInteger $ ceiling n
ceil wrong = invalidTypeOfArgument "ceil" wrong

floor :: Value -> Either String Value
floor (Value (Aeson.Number n)) = Right $ Value $ Aeson.Number $ fromInteger $ Prelude.floor n
floor wrong = invalidTypeOfArgument "floor" wrong

-- string functions

endsWith :: Value -> Value -> Either String Value
endsWith (Value (Aeson.String str)) (Value (Aeson.String suffix)) = Right $ bool $ Text.isSuffixOf suffix str
endsWith (Value (Aeson.String _)) wrong = invalidTypeOfArgument "ends_with" wrong
endsWith wrong _ = invalidTypeOfArgument "ends_with" wrong

join :: Value -> Value -> Either String Value
join (Value glue@(Aeson.String _)) (Value (Aeson.Array strings)) = Value <$> foldrM concatAeson emptyString interspersedStrings
  where
    interspersedStrings = intersperseVector glue strings
    emptyString = Aeson.String Text.empty
join (Value (Aeson.String _)) wrong = invalidTypeOfArgument "join" wrong
join wrong _ = invalidTypeOfArgument "join" wrong

intersperseVector :: a -> Vector a -> Vector a
intersperseVector glue values
    | Vector.length values < 2 = values
    | otherwise = Vector.cons head $ Vector.cons glue $ intersperseVector glue tail
  where
    head = Vector.head values
    tail = Vector.tail values

concatAeson :: Aeson.Value -> Aeson.Value -> Either String Aeson.Value
concatAeson (Aeson.String left) (Aeson.String right) = Right $ Aeson.String $ Text.append left right
concatAeson wrong (Aeson.String _) = Left $ "join: invalid type of value '" ++ ByteString.unpack (Aeson.encode wrong) ++ "'"
concatAeson _ _ = Left "join: invalid type of values"

-- array functions

mapExpression :: Value -> Value -> Either String Value
mapExpression (Expression fn) (Value (Aeson.Array values)) = do
    mappedValues <- Vector.mapM (aesonFn fn) values
    return $ Value $ Aeson.Array mappedValues
mapExpression (Expression _) wrong = invalidTypeOfArgument "map" wrong
mapExpression wrong _ = invalidTypeOfArgument "map" wrong

max :: Value -> Either String Value
max (Value (Aeson.Array values)) = Value <$> foldrM maxAeson Aeson.Null values
max wrong = invalidTypeOfArgument "max" wrong

maxAeson :: Aeson.Value -> Aeson.Value -> Either String Aeson.Value
maxAeson other Aeson.Null = Right other
maxAeson (Aeson.Number left) (Aeson.Number right) = Right $ Aeson.Number $ Prelude.max left right
maxAeson (Aeson.String left) (Aeson.String right) = Right $ Aeson.String $ Prelude.max left right
maxAeson wrong _  = Left $ "max: invalid type of value '" ++ ByteString.unpack (Aeson.encode wrong) ++ "'"

-- other functions

contains :: Value -> Value -> Either String Value
contains (Value (Aeson.Array values)) value = Right $ bool $ Vector.elem (toAeson value) values
contains (Value (Aeson.String str)) (Value (Aeson.String substr)) = Right $ bool $ Text.isInfixOf substr str
contains (Value (Aeson.String _)) wrong = invalidTypeOfArgument "contains" wrong
contains wrong _ = invalidTypeOfArgument "contains" wrong

keys :: Value -> Either String Value
keys (Value (Aeson.Object obj)) = Right $ Value $ Aeson.Array $ Vector.fromList $ map Aeson.String $ HashMap.keys obj
keys wrong = invalidTypeOfArgument "keys" wrong

length :: Value -> Either String Value
length (Value value) = do
    len <- aesonLength value
    return $ Value $ Aeson.Number $ Scientific.scientific len 0
length _ = Right $ Value $ Aeson.Number $ Scientific.scientific 0 0

aesonLength :: Aeson.Value -> Either String Integer
aesonLength (Aeson.Array values) = Right $ fromIntegral $ Vector.length values
aesonLength (Aeson.String value) = Right $ fromIntegral $ Text.length value
aesonLength (Aeson.Object values) = Right $ fromIntegral $ HashMap.size values
aesonLength wrong = Left $ "length: invalid type of argument '" ++ ByteString.unpack (Aeson.encode wrong) ++ "'"

-- to/from Aeson

toAeson :: Value -> Aeson.Value
toAeson (Value v) = v
toAeson _ = Aeson.Null

fromAeson :: Aeson.Value -> Value
fromAeson = Value

aesonFn :: Monad m => (Value -> m Value) -> (Aeson.Value -> m Aeson.Value)
aesonFn fn = fmap toAeson . fn . fromAeson

-- errors

invalidTypeOfArgument :: String -> Value -> Either String a
invalidTypeOfArgument fn arg = Left $ fn ++ ": invalid type of argument '" ++ encodeString arg ++ "'"
