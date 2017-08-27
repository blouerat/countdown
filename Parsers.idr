module Parsers

import Control.Monad.State
import Text.Lexer

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
data Expression = Number Int Int
                | Brackets Expression Int
                | Operation Operator Expression Expression Int

export
Show Expression where
  show (Number n pos)         = show n
  show (Brackets x pos)       = "(" ++ show x ++ ")"
  show (Operation op l r pos) = show l ++ " " ++ show op ++ " " ++ show r

record Siding where
       constructor MkSiding
       base : List (Operator, Int)
       brackets : List (Int, List (Operator, Int))

record Wye where
       constructor MkWye
       output : List Expression
       siding : Siding

empty : Wye
empty = MkWye [] (MkSiding [] [])

NonEmptyList : Type -> Type
NonEmptyList a = (a, List a)

error : Int -> String -> StateT Wye (Either Error) ty
error pos msg = lift (Left (MkError pos msg))

validNumbers : List Int
validNumbers = [1..10] ++ [25, 50, 75, 100]

number : Int -> Int -> StateT Wye (Either Error) ()
number k pos = if elem k validNumbers
                  then modify (record { output $= ((Number k pos) ::) })
                  else error pos ("Invalid number " ++ show k ++ ", expected one of: " ++ show validNumbers)

openBracket : Int -> StateT Wye (Either Error) ()
openBracket pos = modify (record { siding -> brackets $= ((pos, []) ::) })

applyOperator : List Expression -> Operator -> Int -> Either Error (List Expression)
applyOperator (r :: (l :: exps)) op pos = Right (Operation op l r pos :: exps)
applyOperator _ _ pos = Left (MkError pos "huh, don't know what happened :/")

applyOperators : List Expression -> (Operator, Int) -> List (Operator, Int) -> Either Error (List Expression)
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

wrap : Int -> List Expression -> List Expression
wrap _ [] = []
wrap bPos ((Number n pos) :: xs) = Brackets (Number n pos) bPos :: xs
wrap bPos ((Brackets x _) :: xs) = (Brackets x bPos) :: xs
wrap bPos ((Operation op l r pos) :: xs) = Brackets (Operation op l r pos) bPos :: xs

closeBracket : Int -> StateT Wye (Either Error) ()
closeBracket pos =
  do y <- get
     case brackets (siding y) of
          [] => error pos "No matching open bracket"
          ((pos, []) :: brackets) => modify (record { output $= wrap pos, siding -> brackets = brackets })
          ((pos, op :: ops) :: brackets) => do output' <- lift (applyOperators (output y) op ops)
                                               modify (record { output = wrap pos output', siding -> brackets = brackets })

terminate : StateT Wye (Either Error) Expression
terminate =
  do y <- get
     case siding y of
          (MkSiding [] []) => onlyOne (output y)
          (MkSiding (op :: ops) []) => do output' <- lift (applyOperators (output y) op ops)
                                          onlyOne output'
          (MkSiding _ ((pos, _) :: _)) => error pos "Missing closing bracket"
  where
    onlyOne : List Expression -> StateT Wye (Either Error) Expression
    onlyOne [] = error 0 "Nothign left in my stack, that's weird"
    onlyOne (exp :: []) = pure exp
    onlyOne (x :: xs) = error 0 "More than one result, odd"

eofError : Int -> StateT Wye (Either Error) ty
eofError lastPos = error (lastPos + 1) "Unexpected EOF, expected a number or an opening bracket"

mutual
  shuntingYard : (Token, Int) -> List (Token, Int) -> StateT Wye (Either Error) Expression
  shuntingYard (TkSpaces, pos)      []            = eofError pos
  shuntingYard (TkSpaces, pos)      (tok :: toks) = shuntingYard tok toks
  shuntingYard (TkNumber k, pos)    toks          = (number k pos) *> shuntingYard' toks
  shuntingYard (TkOpenBracket, pos) []            = eofError pos
  shuntingYard (TkOpenBracket, pos) (tok :: toks) = (openBracket pos) *> shuntingYard tok toks
  shuntingYard (tok, pos)           _             = error pos ("Unexpected token: " ++ show tok ++ ", expected a number or an opening bracket")

  shuntingYard' : List (Token, Int) -> StateT Wye (Either Error) Expression
  shuntingYard' []                                    = terminate
  shuntingYard' ((TkOperator _, pos) :: [])           = eofError pos
  shuntingYard' ((TkOperator op, pos) :: tok :: toks) = (operator op pos) *> shuntingYard tok toks
  shuntingYard' ((TkSpaces, _) :: toks)               = shuntingYard' toks
  shuntingYard' ((TkCloseBracket, pos) :: toks)       = (closeBracket pos) *> shuntingYard' toks
  shuntingYard' ((tok, pos) :: _)                     = error pos ("Unexpected token: " ++ show tok ++ ", expected an operator or a closing bracket")

intDiv : Int -> Int -> Int -> Either Error Int
intDiv x 0 pos = Left (MkError pos "div by 0")
intDiv x y pos = if assert_total (mod x y == 0)
                    then Right (assert_total (div x y))
                    else Left (MkError pos "not int div")

eval : Expression -> Either Error Int
eval (Number n _) = Right n
eval (Brackets exp _) = eval exp
eval (Operation Add l r _) = liftA2 (+) (eval l) (eval r)
eval (Operation Subtract l r pos) = do l' <- eval l
                                       r' <- eval r
                                       if l' > r'
                                          then Right (l' - r')
                                          else Left (MkError pos ("Cannot subtract (" ++ show r ++ " = " ++ show r' ++ ") from (" ++ show l ++ " = " ++ show l' ++ ")"))
eval (Operation Multiply l r _) = liftA2 (*) (eval l) (eval r)
eval (Operation Divide l r pos) = do l' <- eval l
                                     r' <- eval r
                                     if r' == 0
                                        then Left (MkError pos ("Cannot divide by (" ++ show r ++ " = 0)"))
                                        else if assert_total (mod l' r' == 0)
                                                then Right (assert_total (div l' r'))
                                                else Left (MkError pos ("Cannot divide (" ++ show l ++ " = " ++ show l' ++ ") by (" ++ show r ++ " = " ++ show r' ++ ")"))

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
               exp <- map fst (runStateT (shuntingYard tok toks) empty)
               res <- eval exp
               Right (exp, res)
