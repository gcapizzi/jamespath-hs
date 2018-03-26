{
module JMESPath.Parser (
  parseExpression,
) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LaxyText.Encoding

import JMESPath.Lexer
import JMESPath.Ast
}

-- Entry point
%name expression

-- Lexer structure
%tokentype { Token }

-- Parser monad
%monad { Either String } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    UNQUOTED_STRING { TokenUnquotedString $$ }
    QUOTED_STRING { TokenQuotedString $$ }
    NUMBER { TokenNumber $$ }
    '.' { TokenDot }
    '[' { TokenOpenSquare }
    ']' { TokenClosedSquare }
    '*' { TokenStar }
%%

Expression : FirstSimpleExpression { $1 }
           | FirstSimpleExpression '[' '*' ']' RestExpression { ProjectExpression $1 $5 }
           | '[' '*' ']' { ProjectExpression Root Root }
           | '[' '*' ']' RestExpression { ProjectExpression Root $4 }

RestExpression : SimpleExpression { $1 }
               | SimpleExpression '[' '*' ']' RestExpression { ProjectExpression $1 $5 }

SimpleExpression : '.' Identifier { SubExpression $2 Root }
                 | SimpleExpression '.' Identifier { SubExpression $3 $1 }
                 | '[' NUMBER ']' { IndexExpression $2 Root }
                 | SimpleExpression '[' NUMBER ']' { IndexExpression $3 $1 }

FirstSimpleExpression : Identifier { SubExpression $1 Root }
                      | FirstSimpleExpression '.' Identifier { SubExpression $3 $1 }
                      | '[' NUMBER ']' { IndexExpression $2 Root }
                      | FirstSimpleExpression '[' NUMBER ']' { IndexExpression $3 $1 }

Identifier : String { Identifier $1 }

String : UNQUOTED_STRING { Text.pack $1 }
       | QUOTED_STRING {% Aeson.eitherDecode $ LaxyText.Encoding.encodeUtf8 $ LazyText.pack $1 }

{
parseError :: [Token] -> Either String a
parseError (l:ls) = Left (show l)
parseError [] = Left "Unexpected end of Input"

parseExpression :: String -> Either String Expression
parseExpression input = do
  tokenStream <- scanTokens input
  expression tokenStream
}
