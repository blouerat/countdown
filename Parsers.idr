module Parsers

import Control.Monad.State
import Text.Lexer
import LazyList

%default total

data Operator = Add | Subtract | Multiply | Divide

Eq Operator where
  (==) Add      Add      = True
  (==) Subtract Subtract = True
  (==) Multiply Multiply = True
  (==) Divide   Divide   = True
  (==) _        _        = False

Show Operator where
  show Add      = "+"
  show Subtract = "-"
  show Multiply = "*"
  show Divide   = "/"

precedence : Operator -> Nat
precedence Add      = 0
precedence Subtract = 0
precedence Multiply = 1
precedence Divide   = 1

data Token = TkOperator Operator
           | TkSpaces
           | TkNumber Int
           | TkOpenBracket
           | TkCloseBracket

Eq Token where
  (==) (TkOperator o1) (TkOperator o2) = o1 == o2
  (==) TkSpaces        TkSpaces        = True
  (==) (TkNumber n1)   (TkNumber n2)   = n1 == n2
  (==) TkOpenBracket   TkOpenBracket   = True
  (==) TkCloseBracket  TkCloseBracket  = True
  (==) _               _               = False

Show Token where
  show (TkOperator op) = show op
  show TkSpaces        = "\\s"
  show (TkNumber k)    = show k
  show TkOpenBracket   = "("
  show TkCloseBracket  = ")"

showLexer : Token -> (Lexer, String -> Token)
showLexer token = (exact (show token), const token)

maybe : Lexer -> Recognise False
maybe l = l <|> empty

maybeIs : Char -> Recognise False
maybeIs = maybe . is

tokenMap : TokenMap Token
tokenMap = [
  showLexer (TkOperator Add),
  showLexer (TkOperator Subtract),
  showLexer (TkOperator Multiply),
  showLexer (TkOperator Divide),
  (some space, const TkSpaces),
  (digits, TkNumber . cast),
  showLexer TkOpenBracket,
  showLexer TkCloseBracket
]

export
record Error where
  constructor MkError
  position : Int
  message : String

export
data Expression = Number Int
                | Brackets Expression
                | Operation Operator Expression Expression

export
Show Expression where
  show (Number n)         = show n
  show (Brackets x)       = "(" ++ show x ++ ")"
  show (Operation op l r) = show l ++ " " ++ show op ++ " " ++ show r

record Siding where
       constructor MkSiding
       base : List (Operator, Int)
       brackets : List (Int, List (Operator, Int))

record Wye where
       constructor MkWye
       output : List (Expression, Int)
       siding : Siding

empty : Wye
empty = MkWye [] (MkSiding [] [])

error : Int -> String -> StateT Wye (Either Error) ty
error pos msg = lift (Left (MkError pos msg))

validNumbers : List Int
validNumbers = [1..10] ++ [25, 50, 75, 100]

numbers : Expression -> LazyList Int
numbers (Number i) = [i]
numbers (Brackets x) = numbers x
numbers (Operation _ l r) = append (numbers l) (numbers r)

allNumbers : List (Expression, Int) -> LazyList Int
allNumbers = foldl (\acc => append acc . numbers . fst) Nil

countMaxN : Nat -> List (Expression, Int) -> Int -> Error -> Either Error Int
countMaxN max xs k err = if length (take max (filter (== k) (allNumbers xs))) < max
                            then Right k
                            else Left err

once : List (Expression, Int) -> Int -> Int -> Either Error Int
once xs k pos = countMaxN 1 xs k (MkError pos ("Number " ++ show k ++ " has already been used"))

twice : List (Expression, Int) -> Int -> Int -> Either Error Int
twice xs k pos = countMaxN 2 xs k (MkError pos ("Number " ++ show k ++ " has already been used twice"))

validateNumber : List (Expression, Int) -> Int -> Int -> Either Error Int
validateNumber xs k pos =
  if elem k [1..10]
     then twice xs k pos
     else if elem k [25, 50, 75, 100]
             then once xs k pos
             else Left (MkError pos ("Invalid number " ++ show k ++ ", expected one of: " ++ show validNumbers))

number : Int -> Int -> StateT Wye (Either Error) ()
number k pos = do y <- get
                  case validateNumber (output y) k pos of
                       Left err => lift (Left err)
                       Right k' => modify (record { output $= ((Number k', k') ::) })

openBracket : Int -> StateT Wye (Either Error) ()
openBracket pos = modify (record { siding -> brackets $= ((pos, []) ::) })

applyOperator : List (Expression, Int) -> Operator -> Int -> Either Error (List (Expression, Int))
applyOperator ((r, r') :: ((l, l') :: exps)) Add _ = Right ((Operation Add l r, l' + r') :: exps)
applyOperator ((r, r') :: ((l, l') :: exps)) Subtract pos =
  if l' > r'
     then Right ((Operation Subtract l r, l' - r') :: exps)
     else Left (MkError pos ("Cannot subtract (" ++ show r ++ " = " ++ show r' ++ ") from (" ++ show l ++ " = " ++ show l' ++ ")"))
applyOperator ((r, r') :: ((l, l') :: exps)) Multiply _ = Right ((Operation Multiply l r, l' * r') :: exps)
applyOperator ((r, r') :: ((l, l') :: exps)) Divide pos =
  if r' == 0
    then Left (MkError pos ("Cannot divide by (" ++ show r ++ " = 0)"))
    else if assert_total (mod l' r' == 0)
            then Right ((Operation Divide l r, assert_total (div l' r')) :: exps)
            else Left (MkError pos ("Cannot divide (" ++ show l ++ " = " ++ show l' ++ ") by (" ++ show r ++ " = " ++ show r' ++ ")"))
applyOperator _ _ pos = Left (MkError pos "huh, don't know what happened :/")

applyOperators : List (Expression, Int) -> (Operator, Int) -> List (Operator, Int) -> Either Error (List (Expression, Int))
applyOperators output (op, pos) [] = applyOperator output op pos
applyOperators output (op, pos) (op' :: ops) = do output' <- applyOperator output op pos
                                                  applyOperators output' op' ops

topSiding : Wye -> List (Operator, Int)
topSiding (MkWye output (MkSiding base [])) = base
topSiding (MkWye output (MkSiding base ((pos, []) :: brackets))) = []
topSiding (MkWye output (MkSiding base ((pos, ops) :: brackets))) = ops

pushOperator : Operator -> Int -> Wye -> Wye
pushOperator op pos (MkWye output (MkSiding base [])) = (MkWye output (MkSiding ((op, pos) :: base) []))
pushOperator op pos (MkWye output (MkSiding base ((bPos, ops) :: brackets))) = (MkWye output (MkSiding base ((bPos, ((op, pos) :: ops)) :: brackets)))

setOperator : Operator -> Int -> Wye -> Wye
setOperator op pos (MkWye output (MkSiding base [])) = (MkWye output (MkSiding [(op, pos)] []))
setOperator op pos (MkWye output (MkSiding base ((bPos, ops) :: brackets))) = (MkWye output (MkSiding base ((bPos, [(op, pos)]) :: brackets)))

operator : Operator -> Int -> StateT Wye (Either Error) ()
operator op pos =
  do y <- get
     case topSiding y of
          [] => modify (pushOperator op pos)
          ((top, topPos) :: ops) => if precedence op > precedence top
                                       then modify (pushOperator op pos)
                                       else case applyOperators (output y) (top, topPos) ops of
                                                 Left err => lift (Left err)
                                                 Right output' => modify ((setOperator op pos) . record { output = output'})

wrap : List (Expression, Int) -> List (Expression, Int)
wrap [] = []
wrap ((Number n, res) :: xs) = (Brackets (Number n), res) :: xs
wrap ((Brackets x, res) :: xs) = (Brackets x, res) :: xs
wrap ((Operation op l r, res) :: xs) = (Brackets (Operation op l r), res) :: xs

closeBracket : Int -> StateT Wye (Either Error) ()
closeBracket pos =
  do y <- get
     case brackets (siding y) of
          [] => error pos "No matching open bracket"
          ((pos, []) :: brackets) => modify (record { output $= wrap, siding -> brackets = brackets })
          ((pos, op :: ops) :: brackets) => do output' <- lift (applyOperators (output y) op ops)
                                               modify (record { output = wrap output', siding -> brackets = brackets })

terminate : StateT Wye (Either Error) (Expression, Int)
terminate =
  do y <- get
     case siding y of
          (MkSiding [] []) => onlyOne (output y)
          (MkSiding (op :: ops) []) => do output' <- lift (applyOperators (output y) op ops)
                                          onlyOne output'
          (MkSiding _ ((pos, _) :: _)) => error pos "Missing closing bracket"
  where
    onlyOne : List (Expression, Int) -> StateT Wye (Either Error) (Expression, Int)
    onlyOne [] = error 0 "Nothign left in my stack, that's weird"
    onlyOne (exp :: []) = pure exp
    onlyOne (x :: xs) = error 0 "More than one result, odd"

eofError : Int -> StateT Wye (Either Error) ty
eofError lastPos = error (lastPos + 1) "Unexpected EOF, expected a number or an opening bracket"

mutual
  shuntingYard : (Token, Int) -> List (Token, Int) -> StateT Wye (Either Error) (Expression, Int)
  shuntingYard (TkSpaces, pos)      []            = eofError pos
  shuntingYard (TkSpaces, pos)      (tok :: toks) = shuntingYard tok toks
  shuntingYard (TkNumber k, pos)    toks          = (number k pos) *> shuntingYard' toks
  shuntingYard (TkOpenBracket, pos) []            = eofError pos
  shuntingYard (TkOpenBracket, pos) (tok :: toks) = (openBracket pos) *> shuntingYard tok toks
  shuntingYard (tok, pos)           _             = error pos ("Unexpected token: " ++ show tok ++ ", expected a number or an opening bracket")

  shuntingYard' : List (Token, Int) -> StateT Wye (Either Error) (Expression, Int)
  shuntingYard' []                                    = terminate
  shuntingYard' ((TkOperator _, pos) :: [])           = eofError pos
  shuntingYard' ((TkOperator op, pos) :: tok :: toks) = (operator op pos) *> shuntingYard tok toks
  shuntingYard' ((TkSpaces, _) :: toks)               = shuntingYard' toks
  shuntingYard' ((TkCloseBracket, pos) :: toks)       = (closeBracket pos) *> shuntingYard' toks
  shuntingYard' ((tok, pos) :: _)                     = error pos ("Unexpected token: " ++ show tok ++ ", expected an operator or a closing bracket")

lex : String -> Either Error ((Token, Int), List (Token, Int))
lex input = case lex tokenMap input of
                 (tokens, (_, _, "")) => case map tokenPos tokens of
                                              [] => Left (MkError 0 "Invalid character")
                                              tok :: toks => Right (tok, toks)
                 (_, (_, col, _)) => Left (MkError col "Invalid character")
  where
    tokenPos : (TokenData Token) -> (Token, Int)
    tokenPos (MkToken line col tok) = (tok, col)

export
run : String -> Either Error (Expression, Int)
run input = do (tok, toks) <- lex input
               map fst (runStateT (shuntingYard tok toks) empty)
