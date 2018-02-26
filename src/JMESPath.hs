module JMESPath
    ( search
    ) where

import JMESPath.Core
import JMESPath.Parser

import Data.Aeson
import Data.ByteString.Lazy
import Data.Text

search :: Text -> ByteString -> Either String ByteString
search query document = do
    queryExpression <- parseExpression query
    documentValue <- eitherDecode document :: Either String Value
    foundValue <- searchValue queryExpression documentValue
    return $ encode foundValue
