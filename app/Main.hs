module Main where

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Lazy.Char8 as ByteString.Char8
import Data.Either
import System.Environment
import System.IO

import JMESPath

main :: IO ()
main = do
    args <- getArgs
    let query = head args
    document <- ByteString.getContents
    let result = search query document
    if isRight result
    then ByteString.Char8.putStrLn $ fromRight ByteString.empty result
    else hPutStrLn stderr $ fromLeft "" result
