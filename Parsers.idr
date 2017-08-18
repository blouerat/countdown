module Parsers

import Control.Monad.State
import Data.Vect

%default total

export
data Error = EOF | Invalid Char | Remaining String | StackOverflow

export
Show Error where
  show EOF = "EOF"
  show (Invalid c) = "Invalid character: " ++ singleton c
  show (Remaining input) = "Remaining input: " ++ input
  show StackOverflow = "Max depth exceeded"

export
data Parser : Type -> Type where
     Step : Parser Char
     Fail : (error : Error) -> Parser ty
     Pure : ty -> Parser ty
     Or : (parser1 : Parser a) -> (parser2 : Parser a) -> Parser a
     Loop : Inf (Parser a) -> Parser a
     (>>=) : (parser : Parser a) -> (f : a -> Parser b) -> Parser b
     Spaces : Parser ()

map : (f : a -> b) -> Parser a -> Parser b
map f parser = do val <- parser
                  Pure (f val)

value : (a -> Char) -> a -> Parser a
value f val = do c <- Step
                 if (c == f val) then Pure val else Fail (Invalid c)

optional : (parser : Parser a) -> Parser (Maybe a)
optional parser = map Just parser `Or` Pure Nothing

chain : Parser a -> Parser b -> Parser (a, b)
chain parserA parserB = do valA <- parserA
                           valB <- parserB
                           Pure (valA, valB)

oneOf : Vect (S k) (Parser a) -> Parser a
oneOf {k = Z} (x :: []) = x
oneOf {k = (S k')} (x :: xs) = Or x (oneOf xs)

buildParser : (maxDepth : Nat) -> Parser ty -> StateT String (Either Error) ty
buildParser _ Step = do input <- get
                        case strM input of
                             StrNil => lift (Left EOF)
                             StrCons x xs => put xs *> pure x
buildParser _ (Fail error) = lift (Left error)
buildParser _ (Pure val) = pure val
buildParser n (Or parser1 parser2)
    = do input <- get
         case runStateT (buildParser n parser1) input of
              Left StackOverflow  => lift (Left StackOverflow) -- StackOverflow shouldn't be a parsing error
              Left _              => put input *> buildParser n parser2
              Right (val, input') => put input' *> pure val
buildParser n (parser >>= f) = do res <- buildParser n parser
                                  buildParser n (f res)
buildParser _ Spaces = modify ltrim -- probably not a primitive as ltrim uses strM under the hood
buildParser (S k) (Loop parser) = buildParser k parser
buildParser Z _ = lift (Left StackOverflow)

export
run : Nat -> Parser ty -> String -> Either Error ty
run n parser input = case runStateT (buildParser n parser) input of
                          Left error => Left error
                          Right (result, "") => Right result
                          Right (_, input) => Left (Remaining input)

char : Char -> Parser Char
char = value id

string : String -> Parser String
string s with (strM s)
  string "" | StrNil = Pure ""
  string (strCons x xs) | (StrCons _ _) = do x' <- char x
                                             xs' <- assert_total (string xs)
                                             Pure (strCons x' xs')

nat : Nat -> Parser Nat
nat n = map (const n) (string (show n))

oneOfNat : Vect (S k) Nat -> Parser Nat
oneOfNat = oneOf . map nat

number : Parser Nat
number = oneOf [
           oneOfNat [100, 10, 1],
           oneOfNat [25, 2],
           oneOfNat [50, 5],
           oneOfNat [75, 7],
           oneOfNat [3, 4, 6, 8, 9]
         ]

data Operator = Add | Substract | Multiply | Divide

-- see https://stackoverflow.com/questions/45716403/idiomatic-way-of-listing-elements-of-a-sum-type-in-idris
Operators : Vect 4 Operator
Operators = [Add, Substract, Multiply, Divide]

total
opInOps : Elem op Operators
opInOps {op = Add} = Here
opInOps {op = Substract} = There Here
opInOps {op = Multiply} = There (There Here)
opInOps {op = Divide} = There (There (There Here))

operatorChar : Operator -> Char
operatorChar Add = '+'
operatorChar Substract = '-'
operatorChar Multiply = '*'
operatorChar Divide = '/'

operator : Parser Operator
operator = oneOf (map (value operatorChar) Operators)

export
data Expression = Number Nat | Brackets Expression | Operation Operator Expression Expression

export
Show Expression where
  show (Number n) = show n
  show (Brackets e) = "(" ++ show e ++ ")"
  show (Operation op left right) = show left ++ " " ++ singleton (operatorChar op) ++ " " ++ show right

mutual
  export
  expression : Parser Expression
  expression = do Spaces
                  left <- Or brackets (map Number number)
                  Spaces
                  Or (operation left) (Pure left)

  brackets : Parser Expression
  brackets = do char '('
                e <- Loop expression
                char ')'
                Pure (Brackets e)

  operation : Expression -> Parser Expression
  operation left = do op <- operator
                      right <- Loop expression
                      Pure (Operation op left right)
