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
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

noSpaceConsumer :: Parser ()
noSpaceConsumer = L.space empty empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

dot :: Parser (Tokens Text)
dot = L.symbol spaceConsumer "."

signedInt :: Parser Int
signedInt = L.signed noSpaceConsumer $ lexeme L.decimal

openSquare :: Parser (Tokens Text)
openSquare = L.symbol spaceConsumer "["

closedSquare :: Parser (Tokens Text)
closedSquare = L.symbol spaceConsumer "]"

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

unquotedString :: Parser Text
unquotedString = T.cons <$> (letterChar <|> char '_') <*> (T.pack <$> many (alphaNumChar <|> char '_'))

quotedString :: Parser Text
quotedString = T.pack <$> between (char '"') (char '"') (some (unescapedChar <|> escapedChar))

identifier :: Parser Expression
identifier = lexeme $ Identifier <$> (quotedString <|> unquotedString)

subExpression :: Parser Expression
subExpression = dot >> identifier

indexExpression :: Parser Expression
indexExpression = IndexExpression <$> between openSquare closedSquare signedInt

expression :: Parser Expression
expression = do
    first <- indexExpression <|> identifier
    subs <- many subExpression
    eof
    return $ foldl SubExpression first subs

parseExpression :: Text -> Either String Expression
parseExpression expressionText = first parseErrorPretty $ parse expression "" expressionText
