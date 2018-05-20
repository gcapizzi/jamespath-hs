module JMESPath.Function
    (
    call
    ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Maybe
import qualified JMESPath.Json as Json

data Function = Function
    { arity :: Int
    , run :: [Json.Value] -> Either String Json.Value
    }

functions :: HashMap String Function
functions = HashMap.fromList
    [ ("abs", Function { arity = 1, run = \[n] -> Json.abs n })
    , ("avg", Function { arity = 1, run = \[n] -> Json.avg n })
    , ("contains", Function { arity = 2, run = \[array, element] -> Json.contains array element })
    , ("ceil", Function { arity = 1, run = \[n] -> Json.ceil n })
    , ("ends_with", Function { arity = 2, run = \[string, suffix] -> Json.endsWith string suffix })
    , ("floor", Function { arity = 1, run = \[n] -> Json.floor n })
    , ("join", Function { arity = 2, run = \[glue, strings] -> Json.join glue strings })
    , ("keys", Function { arity = 1, run = \[object] -> Json.keys object })
    , ("length", Function { arity = 1, run = \[value] -> Json.length value })
    , ("map", Function { arity = 2, run = \[fn, value] -> Json.mapExpression fn value })
    , ("max", Function { arity = 1, run = \[values] -> Json.max values })
    ]

getFunction :: String -> Either String Function
getFunction functionName
    | Maybe.isJust function = Right $ Maybe.fromJust function
    | otherwise = Left $ "undefined function '" ++ functionName ++ "'"
      where
        function = HashMap.lookup functionName functions

call :: String -> [Json.Value] -> Either String Json.Value
call functionName args = do
    function <- getFunction functionName
    if length args == arity function
        then run function args
        else Left $ functionName ++ ": invalid arity, expected " ++ show (arity function) ++ " argument"
