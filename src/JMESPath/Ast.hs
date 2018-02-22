module JMESPath.Ast
    ( Expression (..)
    ) where

import Data.Text

newtype Expression = Identifier Text
