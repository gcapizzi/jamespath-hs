module JMESPath
    ( search
    ) where

import Data.ByteString.Lazy
import Data.Text
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap

search :: Text -> ByteString -> Either String ByteString
search query document = do
    documentValue <- eitherDecode document :: Either String Value
    foundValue <- searchValue query documentValue
    return $ encode foundValue

searchValue :: Text -> Value -> Either String Value
searchValue query document = Right foundValue
  where
    foundValue = HashMap.lookupDefault Null query object
    object = case document of
        Object o -> o
        _        -> HashMap.empty
  
