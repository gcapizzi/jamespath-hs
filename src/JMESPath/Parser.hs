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
expression = Identifier <$> (quotedIdentifier <|> unquotedIdentifier)

unquotedIdentifier :: Parser Text
unquotedIdentifier = pack <$> many alphaNumChar

quotedIdentifier :: Parser Text
quotedIdentifier = pack <$> between (char '"') (char '"') (many (alphaNumChar <|> char ' '))

parseExpression :: Text -> Either String Expression
parseExpression expressionText = first show $ parse expression "" expressionText
