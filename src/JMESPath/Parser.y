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
    '[]' { TokenOpenClosedSquare }
%%

Expression : FirstExpressionWithProjections { $1 }
           | '[]' { FlattenExpression Root Root }
           | '[]' ExpressionWithProjections { FlattenExpression Root $2 }
           | Expression '[]' { FlattenExpression $1 Root }
           | Expression '[]' ExpressionWithProjections { FlattenExpression $1 $3 }

FirstExpressionWithProjections : FirstSimpleExpression { $1 }
                               | '[' '*' ']' { ArrayProjectExpression Root Root }
                               | '*' { ObjectProjectExpression Root Root }
                               | '[' '*' ']' ExpressionWithProjections { ArrayProjectExpression Root $4 }
                               | '*' ExpressionWithProjections { ObjectProjectExpression Root $2 }
                               | FirstSimpleExpression '[' '*' ']' { ArrayProjectExpression $1 Root }
                               | FirstSimpleExpression '.' '*' { ObjectProjectExpression $1 Root }
                               | FirstSimpleExpression '[' '*' ']' ExpressionWithProjections { ArrayProjectExpression $1 $5 }
                               | FirstSimpleExpression '.' '*' ExpressionWithProjections { ObjectProjectExpression $1 $4 }

ExpressionWithProjections : SimpleExpression { $1 }
                          | '[' '*' ']' { ArrayProjectExpression Root Root }
                          | '.' '*' { ObjectProjectExpression Root Root }
                          | '[' '*' ']' ExpressionWithProjections { ArrayProjectExpression Root $4 }
                          | '.' '*' ExpressionWithProjections { ObjectProjectExpression Root $3 }
                          | SimpleExpression '[' '*' ']' { ArrayProjectExpression $1 Root }
                          | SimpleExpression '.' '*' { ObjectProjectExpression $1 Root }
                          | SimpleExpression '[' '*' ']' ExpressionWithProjections { ArrayProjectExpression $1 $5 }
                          | SimpleExpression '.' '*' ExpressionWithProjections { ObjectProjectExpression $1 $4 }

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
parseError (l:ls) = Left ("Syntax error: unexpected token '" ++ show l ++ "'")
parseError [] = Left "Syntax error: unexpected end of input"

parseExpression :: String -> Either String Expression
parseExpression input = do
  tokenStream <- scanTokens input
  expression tokenStream
}
