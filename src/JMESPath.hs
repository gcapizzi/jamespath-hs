module JMESPath
    ( search
    ) where

import Data.ByteString.Lazy
import Data.Text
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap

search :: Text -> ByteString -> ByteString
search query document = encode value
  where
    value = HashMap.lookupDefault "" query object
    object = case decodedValue of
        Just (Object o) -> o
        _        -> HashMap.empty
    decodedValue = decode document :: Maybe Value
