module JMESPath
    ( search
    ) where

import JMESPath.Core
import JMESPath.Parser
import JMESPath.Json

import Data.ByteString.Lazy (ByteString)

search :: String -> ByteString -> Either String ByteString
search query document = do
    queryExpression <- parseExpression query
    documentValue <- decode document
    foundValue <- searchValue queryExpression documentValue
    return $ encode foundValue
