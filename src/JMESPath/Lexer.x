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
  \, { \_ -> TokenComma }
  \{ { \_ -> TokenOpenCurly }
  \} { \_ -> TokenClosedCurly }
  \|\| { \_ -> TokenOr }
  \&\& { \_ -> TokenAnd }
  \( { \_ -> TokenOpenParen }
  \) { \_ -> TokenClosedParen }
  \! { \_ -> TokenNot }
  \=\= { \_ -> TokenEqual }
  \!\= { \_ -> TokenNotEqual }
  \< { \_ -> TokenLessThan }
  \> { \_ -> TokenGreaterThan }
  \<\= { \_ -> TokenLessThanOrEqual }
  \>\= { \_ -> TokenGreaterThanOrEqual }
  \[\? { \_ -> TokenOpenSquareQuestionMark }
  `[^`]+` { \s -> TokenJson (tail (init s)) }
  '[^']+' { \s -> TokenJsonRawString (tail (init s)) }

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
  | TokenComma
  | TokenOpenCurly
  | TokenClosedCurly
  | TokenOr
  | TokenAnd
  | TokenOpenParen
  | TokenClosedParen
  | TokenNot
  | TokenEqual
  | TokenNotEqual
  | TokenLessThan
  | TokenGreaterThan
  | TokenLessThanOrEqual
  | TokenGreaterThanOrEqual
  | TokenOpenSquareQuestionMark
  | TokenBacktick
  | TokenJson String
  | TokenJsonRawString String
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
  show (TokenComma) = ","
  show (TokenOpenCurly) = "{"
  show (TokenClosedCurly) = "}"
  show (TokenOr) = "||"
  show (TokenAnd) = "&&"
  show (TokenOpenParen) = "("
  show (TokenClosedParen) = ")"
  show (TokenNot) = "!"
  show (TokenEqual) = "=="
  show (TokenNotEqual) = "!="
  show (TokenLessThan) = "<"
  show (TokenGreaterThan) = ">"
  show (TokenLessThanOrEqual) = "<="
  show (TokenGreaterThanOrEqual) = ">="
  show (TokenOpenSquareQuestionMark) = "[?"
  show (TokenBacktick) = "`"
  show (TokenJson s) = s
  show (TokenJsonRawString s) = s

scanTokens :: String -> Either String [Token]
scanTokens inputString = scan ('\n', [], inputString)

scan :: AlexInput -> Either String [Token]
scan input@(_, _, inputString) = case alexScan input 0 of
    AlexEOF -> return []
    AlexError (_, _, remainingInputString) -> Left ("Syntax error: unexpected input '" ++ remainingInputString ++ "'")
    AlexSkip remainingInput _ -> scan remainingInput
    AlexToken remainingInput tokenLength action -> do
        remainingTokens <- scan remainingInput
        let token = action (take tokenLength inputString)
        return (token:remainingTokens)
}
