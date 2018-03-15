module JMESPath
    ( search
    ) where

import JMESPath.Core
import JMESPath.Parser
import JMESPath.Json

import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)

search :: Text -> ByteString -> Either String ByteString
search query document = do
    queryExpression <- parseExpression query
    documentValue <- decode document
    let foundValue = searchValue queryExpression documentValue
    return $ encode foundValue
