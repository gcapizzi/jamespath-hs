{
module JMESPath.Parser (
  parseExpression,
) where

import qualified Data.Aeson as Aeson
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LaxyText.Encoding

import JMESPath.Lexer
import JMESPath.Ast
}

%name expression
%tokentype { Token }
%monad { Either String } { (>>=) } { return }
%error { parseError }
%errorhandlertype explist
%token
    UNQUOTED_STRING { TokenUnquotedString $$ }
    QUOTED_STRING { TokenQuotedString $$ }
    NUMBER { TokenNumber $$ }
    '.' { TokenDot }
    '[' { TokenOpenSquare }
    ']' { TokenClosedSquare }
    '*' { TokenStar }
    '[]' { TokenOpenClosedSquare }
    ':' { TokenColon }
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
                 | '[' NUMBER ':' NUMBER ']' { SliceExpression $2 $4 Nothing Root }
                 | '[' NUMBER ':' NUMBER ':' NUMBER ']' { SliceExpression $2 $4 (Just $6) Root }
                 | SimpleExpression '[' NUMBER ']' { IndexExpression $3 $1 }
                 | SimpleExpression '[' NUMBER ':' NUMBER ']' { SliceExpression $3 $5 Nothing $1 }
                 | SimpleExpression '[' NUMBER ':' NUMBER ':' NUMBER ']' { SliceExpression $3 $5 (Just $7) $1 }

FirstSimpleExpression : Identifier { SubExpression $1 Root }
                      | FirstSimpleExpression '.' Identifier { SubExpression $3 $1 }
                      | '[' NUMBER ']' { IndexExpression $2 Root }
                      | '[' NUMBER ':' NUMBER ']' { SliceExpression $2 $4 Nothing Root }
                      | '[' NUMBER ':' NUMBER ':' NUMBER ']' { SliceExpression $2 $4 (Just $6) Root }
                      | FirstSimpleExpression '[' NUMBER ']' { IndexExpression $3 $1 }
                      | FirstSimpleExpression '[' NUMBER ':' NUMBER ']' { SliceExpression $3 $5 Nothing $1 }
                      | FirstSimpleExpression '[' NUMBER ':' NUMBER ':' NUMBER ']' { SliceExpression $3 $5 (Just $7) $1 }

Identifier : String { Identifier $1 }

String : UNQUOTED_STRING { Text.pack $1 }
       | QUOTED_STRING {% Aeson.eitherDecode $ LaxyText.Encoding.encodeUtf8 $ LazyText.pack $1 }

{
parseError :: ([Token], [String]) -> Either String a
parseError (l:ls, exp) = Left ("Syntax error: unexpected token '" ++ show l ++ "', expected one of: " ++ commaSeparatedList exp)
parseError ([], _) = Left "Syntax error: unexpected end of input"

commaSeparatedList = List.concat . List.intersperse ", "

parseExpression :: String -> Either String Expression
parseExpression input = do
  tokenStream <- scanTokens input
  expression tokenStream
}
