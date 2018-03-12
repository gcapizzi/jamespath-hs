module JMESPath.Parser
    ( parseExpression
    ) where

import JMESPath.Core

import Data.Bifunctor
import qualified Data.ByteString as B
import Data.List.Split
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Void
import Data.Word
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

unescapedChar :: Parser Text
unescapedChar = T.singleton <$> noneOf ['"', '\\']

escapedChar :: Parser Text
escapedChar = char '\\' >>
        "\"" <$ char '"'
    <|> "\\" <$ char '\\'
    <|> "/"  <$ char '/'
    <|> "\b" <$ char 'b'
    <|> "\f" <$ char 'f'
    <|> "\n" <$ char 'n'
    <|> "\r" <$ char 'r'
    <|> "\t" <$ char 't'

escapedUnicodeSequences :: Parser Text
escapedUnicodeSequences = (TE.decodeUtf16BE . B.pack . concat) <$> some escapedUnicodeSequence

escapedUnicodeSequence :: Parser [Word8]
escapedUnicodeSequence = hexToWord8s <$> (string "\\u" >> count 4 hexDigitChar)

hexToWord8s :: String -> [Word8]
hexToWord8s = map hexToWord8 . chunksOf 2

hexToWord8 :: String -> Word8
hexToWord8 = fst . head . readHex

unquotedString :: Parser Text
unquotedString = T.cons <$> (letterChar <|> char '_') <*> (T.pack <$> many (alphaNumChar <|> char '_'))

quotedString :: Parser Text
quotedString = T.concat <$> between (char '"') (char '"') (some (unescapedChar <|> try escapedChar <|> escapedUnicodeSequences))

identifier :: Parser Expression
identifier = lexeme $ Identifier <$> (quotedString <|> unquotedString)

selector :: Parser (Expression -> Expression)
selector = SubExpression <$> identifier

subExpression :: Parser (Expression -> Expression)
subExpression = dot >> selector

indexExpression :: Parser (Expression -> Expression)
indexExpression = IndexExpression <$> between openSquare closedSquare signedInt

expression :: Parser Expression
expression = do
    firstExpression <- selector <|> indexExpression
    followingExpressions <- many (subExpression <|> indexExpression)
    eof
    return $ foldl (|>) Root (firstExpression:followingExpressions)

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

parseExpression :: Text -> Either String Expression
parseExpression expressionText = first parseErrorPretty $ parse expression "" expressionText
