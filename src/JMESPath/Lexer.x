{
module JMESPath.Lexer (
  Token(..),
  scanTokens
) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$escape = [\x5C]
$quote = [\x22]
$unescapedChar = ~[$escape $quote]
$hexDigit = [0-9a-fA-F]

@escapedChar = $escape (\x22 | \x5C | \x2F | \x62 | \x66 | \x6E | \x72 | \x74 | \x75 $hexDigit{4})

tokens :-
  $white+ ;
  [$alpha \_] [$alpha $digit \_]* { \s -> TokenUnquotedString s }
  $quote ($unescapedChar | @escapedChar)+ $quote { \s -> TokenQuotedString s }
  \-? $digit+ { \s -> TokenNumber (read s) }
  \. { \_ -> TokenDot }
  \[ { \_ -> TokenOpenSquare }
  \] { \_ -> TokenClosedSquare }
  \* { \_ -> TokenStar }
  \[\] { \_ -> TokenOpenClosedSquare }
  \: { \_ -> TokenColon }
  \| { \_ -> TokenPipe }

{
data Token
  = TokenUnquotedString String
  | TokenQuotedString String
  | TokenNumber Int
  | TokenDot
  | TokenOpenSquare
  | TokenClosedSquare
  | TokenStar
  | TokenOpenClosedSquare
  | TokenColon
  | TokenPipe
  deriving (Eq)

instance Show Token where
  show (TokenUnquotedString s) = s
  show (TokenQuotedString s) = s
  show (TokenNumber n) = show n
  show (TokenDot) = "."
  show (TokenOpenSquare) = "["
  show (TokenClosedSquare) = "]"
  show (TokenStar) = "*"
  show (TokenOpenClosedSquare) = "[]"
  show (TokenColon) = ":"
  show (TokenPipe) = "|"

scanTokens :: String -> Either String [Token]
scanTokens input = go ('\n', [], input)
  where
    go inp@(_, _, str) = case alexScan inp 0 of
        AlexEOF -> return []
        AlexError (_, _, rst) -> Left ("Syntax error: unexpected input '" ++ rst ++ "'")
        AlexSkip inp' _ -> go inp'
        AlexToken inp' len act -> do
            res <- go inp'
            let rest = act (take len str)
            return (rest:res)
}
