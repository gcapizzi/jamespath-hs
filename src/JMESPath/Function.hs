module JMESPath.Function
    (
    call
    ) where

import qualified JMESPath.Json as Json

call :: String -> [Json.Value] -> Either String Json.Value
call "abs" [n] = Json.abs n
call "abs" _ = Left "abs: invalid arity, expected one argument"
call "avg" [ns] = Json.avg ns
call "avg" _ = Left "avg: invalid arity, expected one argument"
call functionName _ = Left $ "undefined function '" ++ functionName ++ "'"
