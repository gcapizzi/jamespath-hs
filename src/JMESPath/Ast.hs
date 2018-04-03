module JMESPath.Ast
    ( Expression(..)
    ) where

import Data.Text (Text)

data Expression = Root
                | Identifier Text
                | SubExpression Expression Expression
                | IndexExpression Int Expression
                | ArrayProjectExpression Expression Expression
                | ObjectProjectExpression Expression Expression
    deriving Show
