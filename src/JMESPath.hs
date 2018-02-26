module JMESPath
    ( search
    ) where

import JMESPath.Ast
import JMESPath.Parser

import Data.Aeson
import Data.ByteString.Lazy
import Data.Text
import qualified Data.HashMap.Strict as HashMap

search :: Text -> ByteString -> Either String ByteString
search query document = do
    queryExpression <- parseExpression query
    documentValue <- eitherDecode document :: Either String Value
    foundValue <- searchValue queryExpression documentValue
    return $ encode foundValue

searchValue :: Expression -> Value -> Either String Value
searchValue (Identifier identifier) (Object object) = Right $ HashMap.lookupDefault Null identifier object
searchValue (SubExpression left right) document = searchValue left document >>= searchValue right
searchValue _ _ = Right Null
