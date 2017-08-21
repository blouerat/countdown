module Parsers

import Data.Vect
import Text.Lexer
import Text.Parser

data Token = TkAdd
           | TkSubstract
           | TkMultiply
           | TkDivide
           | TkSpace Nat
           | TkNumber Nat
           | TkOpenBracket
           | TkCloseBracket

Eq Token where
  (==) TkAdd TkAdd = True
  (==) TkSubstract TkSubstract = True
  (==) TkMultiply TkMultiply = True
  (==) TkDivide TkDivide = True
  (==) (TkSpace n1) (TkSpace n2) = n1 == n2
  (==) (TkNumber n1) (TkNumber n2) = n1 == n2
  (==) TkOpenBracket TkOpenBracket = True
  (==) TkCloseBracket TkCloseBracket = True
  (==) _ _ = False

Show Token where
  show TkAdd = "+"
  show TkSubstract = "-"
  show TkMultiply = "*"
  show TkDivide = "/"
  show (TkSpace k) = "\\s"
  show (TkNumber k) = show k
  show TkOpenBracket = "("
  show TkCloseBracket = ")"

showLexer : Token -> (Lexer, String -> Token)
showLexer token = (exact (show token), const token)

maybe : Lexer -> Recognise False
maybe l = l <|> empty

maybeIs : Char -> Recognise False
maybeIs = maybe . is

numberLexer : Lexer
numberLexer = is '1' <+> maybe (is '0' <+> maybeIs '0') <|>
              is '2' <+> maybeIs '5' <|>
              is '3' <|>
              is '4' <|>
              is '5' <+> maybeIs '0' <|>
              is '6' <|>
              is '7' <+> maybeIs '5' <|>
              is '8' <|>
              is '9'

tokenMap : TokenMap Token
tokenMap = [
  showLexer TkAdd,
  showLexer TkSubstract,
  showLexer TkMultiply,
  showLexer TkDivide,
  (some space, TkSpace . length),
  (numberLexer, TkNumber . cast),
  showLexer TkOpenBracket,
  showLexer TkCloseBracket
]

export
data Operator = Add | Substract | Multiply | Divide

export
Show Operator where
  show Add = "+"
  show Substract = "-"
  show Multiply = "*"
  show Divide = "/"

export
data Expression = Number Nat | Brackets Expression | Operation Operator Expression Expression

export
Show Expression where
  show (Number n) = show n
  show (Brackets e) = "(" ++ show e ++ ")"
  show (Operation op left right) = show left ++ " " ++ show op ++ " " ++ show right

Parser : Bool -> Type -> Type
Parser consumes ty = Grammar (TokenData Token) consumes ty

number : Parser True Expression
number = terminal isNumberTk
  where
    isNumberTk : TokenData Token -> Maybe Expression
    isNumberTk (MkToken _ _ (TkNumber n)) = Just (Number n)
    isNumberTk _ = Nothing

exact : Token -> a -> Parser True a
exact token val = terminal (\tk => toMaybe ((tok tk) == token) val)

brackets : Parser True Expression -> Parser True Expression
brackets = between (exact TkOpenBracket ()) (exact TkCloseBracket ())

operator : Parser True Operator
operator = exact TkAdd Add <|>
           exact TkSubstract Substract <|>
           exact TkMultiply Multiply <|>
           exact TkDivide Divide

space : Parser True Token
space = terminal isSpaceTk
  where
    isSpaceTk : TokenData Token -> Maybe Token
    isSpaceTk (MkToken _ _ (TkSpace n)) = Just (TkSpace n)
    isSpaceTk _ = Nothing

mutual
  operation : Expression -> Parser True Expression
  operation left = do op <- operator
                      commit
                      right <- expressionRec
                      pure (Operation op left right)

  expressionRec : Parser True Expression
  expressionRec = do maybe space
                     left <- number <|> brackets expressionRec
                     maybe space
                     optional (operation left) left

expression : Parser True Expression
expression = do res <- expressionRec
                eof
                pure res

public export
record Error where
  constructor MkError
  line : Int
  column : Int
  message : String

runLex : String -> Either Error (List (TokenData Token))
runLex input = case lex tokenMap input of
                    (tokens, (_, _, "")) => Right tokens
                    (_, (line, col, _)) => Left (MkError line col "Invalid character")

runParse : List (TokenData Token) -> Either Error Expression
runParse tokens = case parse tokens expression of
                       (Left error) => Left (parseError error)
                       (Right (exp, remainder)) => Right exp
  where
    parseError : ParseError (TokenData Token) -> Error
    parseError (Error msg []) = MkError 0 0 msg
    parseError (Error msg ((MkToken line col _) :: _)) = MkError line col msg

export
run : String -> Either Error Expression
run input = runLex input >>= runParse
