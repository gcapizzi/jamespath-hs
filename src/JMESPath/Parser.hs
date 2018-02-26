module JMESPath.Parser
    ( parseExpression
    ) where

import JMESPath.Core

import Data.Bifunctor
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Numeric
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

expression :: Parser Expression
expression = do
    id <- identifier
    subs <- many subExpression
    eof
    return $ foldl SubExpression id subs

subExpression :: Parser Expression
subExpression = char '.' >> identifier

identifier :: Parser Expression
identifier = Identifier <$> (quotedString <|> unquotedString)

unquotedString :: Parser Text
unquotedString = T.cons <$> (letterChar <|> char '_') <*> (T.pack <$> many (alphaNumChar <|> char '_'))

quotedString :: Parser Text
quotedString = T.pack <$> between (char '"') (char '"') (some (unescapedChar <|> escapedChar))

unescapedChar :: Parser Char
unescapedChar = noneOf ['"', '\\']

escapedChar :: Parser Char
escapedChar = char '\\' >>
        char '"'
    <|> char '\\'
    <|> char '/'
    <|> '\b' <$ char 'b'
    <|> '\f' <$ char 'f'
    <|> '\n' <$ char 'n'
    <|> '\r' <$ char 'r'
    <|> '\t' <$ char 't'
    <|> (char 'u' >> (hexToChar <$> count 4 hexDigitChar))

hexToChar :: String -> Char
hexToChar = chr . fst . head . readHex

parseExpression :: Text -> Either String Expression
parseExpression expressionText = first parseErrorPretty $ parse expression "" expressionText
