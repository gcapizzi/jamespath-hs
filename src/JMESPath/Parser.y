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
    '|' { TokenPipe }
    ',' { TokenComma }
    '{' { TokenOpenCurly }
    '}' { TokenClosedCurly }
    '||' { TokenOr }
    '&&' { TokenAnd }
    '(' { TokenOpenParen }
    ')' { TokenClosedParen }
    '!' { TokenNot }
    '==' { TokenEqual }
    '!=' { TokenNotEqual }
    '<' { TokenLessThan }
    '>' { TokenGreaterThan }
    '<=' { TokenLessThanOrEqual }
    '>=' { TokenGreaterThanOrEqual }
    '[?' { TokenOpenSquareQuestionMark }
    '`' { TokenBacktick }
    JSON { TokenJson $$ }
    JSON_RAW_STRING { TokenJsonRawString $$ }
    '@' { TokenAt }

%left '||'
%left '&&'
%left '!'
%left '==' '!='
%left '<' '>' '<=' '>='
%%

Expression : FirstExpressionWithProjections { $1 }
           | FirstExpressionWithProjections '|' Expression { PipeExpression $1 $3 }
           | '[]' { FlattenExpression Root Root }
           | '[]' ExpressionWithProjections { FlattenExpression Root $2 }
           | Expression '[]' { FlattenExpression $1 Root }
           | Expression '[]' ExpressionWithProjections { FlattenExpression $1 $3 }
           | Expression '||' Expression { OrExpression $1 $3 }
           | Expression '&&' Expression { AndExpression $1 $3 }
           | '(' Expression ')' { $2 }
           | '!' Expression { NotExpression $2 }
           | Expression '==' Expression { EqualExpression $1 $3 }
           | Expression '!=' Expression { NotEqualExpression $1 $3 }
           | Expression '<' Expression { LessThanExpression $1 $3 }
           | Expression '>' Expression { GreaterThanExpression $1 $3 }
           | Expression '<=' Expression { LessThanOrEqualExpression $1 $3 }
           | Expression '>=' Expression { GreaterThanOrEqualExpression $1 $3 }

FirstExpressionWithProjections : FirstSimpleExpression { $1 }
                               | '[' opt(NUMBER) ':' opt(NUMBER) ']' { SliceExpression $2 $4 Nothing Root Root }
                               | '[' opt(NUMBER) ':' opt(NUMBER) ':' opt(NUMBER) ']' { SliceExpression $2 $4 $6 Root Root }
                               | '[' '*' ']' { ArrayProjectExpression Root Root }
                               | '*' { ObjectProjectExpression Root Root }
                               | '[' opt(NUMBER) ':' opt(NUMBER) ']' ExpressionWithProjections { SliceExpression $2 $4 Nothing Root $6 }
                               | '[' opt(NUMBER) ':' opt(NUMBER) ':' opt(NUMBER) ']' ExpressionWithProjections { SliceExpression $2 $4 $6 Root $8 }
                               | '[' '*' ']' ExpressionWithProjections { ArrayProjectExpression Root $4 }
                               | '*' ExpressionWithProjections { ObjectProjectExpression Root $2 }
                               | FirstSimpleExpression '[' opt(NUMBER) ':' opt(NUMBER) ']' { SliceExpression $3 $5 Nothing $1 Root }
                               | FirstSimpleExpression '[' opt(NUMBER) ':' opt(NUMBER) ':' opt(NUMBER) ']' { SliceExpression $3 $5 $7 $1 Root }
                               | FirstSimpleExpression '[' '*' ']' { ArrayProjectExpression $1 Root }
                               | FirstSimpleExpression '.' '*' { ObjectProjectExpression $1 Root }
                               | FirstSimpleExpression '[' opt(NUMBER) ':' opt(NUMBER) ']' ExpressionWithProjections { SliceExpression $3 $5 Nothing $1 $7 }
                               | FirstSimpleExpression '[' opt(NUMBER) ':' opt(NUMBER) ':' opt(NUMBER) ']' ExpressionWithProjections { SliceExpression $3 $5 $7 $1 $9 }
                               | FirstSimpleExpression '[' '*' ']' ExpressionWithProjections { ArrayProjectExpression $1 $5 }
                               | FirstSimpleExpression '.' '*' ExpressionWithProjections { ObjectProjectExpression $1 $4 }

ExpressionWithProjections : SimpleExpression { $1 }
                          | '[' opt(NUMBER) ':' opt(NUMBER) ']' { SliceExpression $2 $4 Nothing Root Root }
                          | '[' opt(NUMBER) ':' opt(NUMBER) ':' opt(NUMBER) ']' { SliceExpression $2 $4 $6 Root Root }
                          | '[' '*' ']' { ArrayProjectExpression Root Root }
                          | '.' '*' { ObjectProjectExpression Root Root }
                          | '[' opt(NUMBER) ':' opt(NUMBER) ']' ExpressionWithProjections { SliceExpression $2 $4 Nothing Root $6 }
                          | '[' opt(NUMBER) ':' opt(NUMBER) ':' opt(NUMBER) ']' ExpressionWithProjections { SliceExpression $2 $4 $6 Root $8 }
                          | '[' '*' ']' ExpressionWithProjections { ArrayProjectExpression Root $4 }
                          | '.' '*' ExpressionWithProjections { ObjectProjectExpression Root $3 }
                          | SimpleExpression '[' opt(NUMBER) ':' opt(NUMBER) ']' { SliceExpression $3 $5 Nothing $1 Root }
                          | SimpleExpression '[' opt(NUMBER) ':' opt(NUMBER) ':' opt(NUMBER) ']' { SliceExpression $3 $5 $7 $1 Root }
                          | SimpleExpression '[' '*' ']' { ArrayProjectExpression $1 Root }
                          | SimpleExpression '.' '*' { ObjectProjectExpression $1 Root }
                          | SimpleExpression '[' opt(NUMBER) ':' opt(NUMBER) ']' ExpressionWithProjections { SliceExpression $3 $5 Nothing $1 $7 }
                          | SimpleExpression '[' opt(NUMBER) ':' opt(NUMBER) ':' opt(NUMBER) ']' ExpressionWithProjections { SliceExpression $3 $5 $7 $1 $9 }
                          | SimpleExpression '[' '*' ']' ExpressionWithProjections { ArrayProjectExpression $1 $5 }
                          | SimpleExpression '.' '*' ExpressionWithProjections { ObjectProjectExpression $1 $4 }

SimpleExpression : '.' String { KeyExpression $2 Root }
                 | SimpleExpression '.' String { KeyExpression $3 $1 }
                 | '[' NUMBER ']' { IndexExpression $2 Root }
                 | '.' '[' ExpressionList ']' { MultiSelectList $3 Root }
                 | '.' '{' KeyExpressionPairList '}' { MultiSelectHash $3 Root }
                 | '[?' Expression ']' { FilterExpression $2 Root }
                 | SimpleExpression '[' NUMBER ']' { IndexExpression $3 $1 }
                 | SimpleExpression '.' '[' ExpressionList ']' { MultiSelectList $4 $1 }
                 | SimpleExpression '.' '{' KeyExpressionPairList '}' { MultiSelectHash $4 $1 }
                 | SimpleExpression '[?' Expression ']' { FilterExpression $3 $1 }

FirstSimpleExpression : String { KeyExpression $1 Root }
                      | FirstSimpleExpression '.' String { KeyExpression $3 $1 }
                      | '[' NUMBER ']' { IndexExpression $2 Root }
                      | '[' ExpressionList ']' { MultiSelectList $2 Root }
                      | '{' KeyExpressionPairList '}' { MultiSelectHash $2 Root }
                      | '[?' Expression ']' { FilterExpression $2 Root }
                      | FirstSimpleExpression '[' NUMBER ']' { IndexExpression $3 $1 }
                      | FirstSimpleExpression '.' '[' ExpressionList ']' { MultiSelectList $4 $1 }
                      | FirstSimpleExpression '.' '{' KeyExpressionPairList '}' { MultiSelectHash $4 $1 }
                      | FirstSimpleExpression '[?' Expression ']' { FilterExpression $3 $1 }
                      | JSON { JsonExpression $1 }
                      | JSON_RAW_STRING { JsonRawStringExpression $1 }
                      | '@' { CurrentNodeExpression }

ExpressionList : Expression { [$1] }
               | Expression ',' ExpressionList { $1 : $3 }

KeyExpressionPairList : KeyExpressionPair { [$1] }
                      | KeyExpressionPair ',' KeyExpressionPairList { $1 : $3 }

KeyExpressionPair : String ':' Expression { ($1, $3) }

String : UNQUOTED_STRING { Text.pack $1 }
       | QUOTED_STRING {% Aeson.eitherDecode $ LaxyText.Encoding.encodeUtf8 $ LazyText.pack $1 }

opt(p) : p { Just $1 }
       | { Nothing }
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
