module JMESPath.Ast
    ( Expression (..)
    ) where

import Data.Text

data Expression = Identifier Text | SubExpression Expression Expression
