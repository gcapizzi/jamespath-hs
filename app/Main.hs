module Main where

import qualified Data.ByteString.Lazy as B hiding (putStrLn)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Either
import qualified Data.Text as T
import System.Environment

import JMESPath

main :: IO ()
main = do
    args <- getArgs
    let query = T.pack $ head args
    document <- B.getContents
    let result = search query document
    if isLeft result
    then putStrLn $ fromLeft "" result
    else B.putStrLn $ fromRight B.empty result
