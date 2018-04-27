module JMESPath.Ast
    ( Expression(..)
    ) where

import Data.Text (Text)

data Expression = Root
                | KeyExpression Text Expression
                | IndexExpression Int Expression
                | SliceExpression (Maybe Int) (Maybe Int) (Maybe Int) Expression Expression
                | ArrayProjectExpression Expression Expression
                | ObjectProjectExpression Expression Expression
                | FlattenExpression Expression Expression
                | PipeExpression Expression Expression
                | MultiSelectList [Expression] Expression
                | MultiSelectHash [(Text, Expression)] Expression
                | OrExpression Expression Expression
                | AndExpression Expression Expression
                | NotExpression Expression
                | EqualExpression Expression Expression
    deriving Show
