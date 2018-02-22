module JMESPath.Parser
    ( parseExpression
    ) where

import JMESPath.Ast

import Data.Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Bifunctor

type Parser = Parsec Void Text

expression :: Parser Expression
expression = do
  e <- quotedIdentifier <|> unquotedIdentifier
  return $ Identifier $ pack e

unquotedIdentifier = many alphaNumChar
quotedIdentifier = between doubleQuote doubleQuote (many (alphaNumChar <|> char ' '))
doubleQuote = char '"'

parseExpression :: Text -> Either String Expression
parseExpression expressionText = first show $ parse expression "" expressionText
