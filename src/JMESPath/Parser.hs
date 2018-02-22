module JMESPath.Parser
    ( parseExpression
    ) where

import JMESPath.Ast

import Data.Bifunctor
import Data.Char
import Data.Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

expression :: Parser Expression
expression = Identifier <$> (quotedString <|> unquotedString)

unquotedString :: Parser Text
unquotedString = pack <$> many alphaNumChar

quotedString :: Parser Text
quotedString = pack <$> between (char '"') (char '"') (many unescapedChar)

unescapedChar :: Parser Char
unescapedChar = noneOf ['"', '\\']

parseExpression :: Text -> Either String Expression
parseExpression expressionText = first show $ parse expression "" expressionText
