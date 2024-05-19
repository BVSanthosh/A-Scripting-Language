{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Expr where

import Data.Char
import Data.Either

import Text.Read
import Data.Fixed
import Data.Maybe
import GHC.Float (powerFloat)
import Parsing
import Data.Function
import Tree
import Outcome

-- The following section of code was modified by kb267
-- At first, 'Expr' contains only addition, conversion to strings, and integer
-- values. You will need to add other operations, and variables
data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pwr Expr Expr
  | Log Expr Expr
  | Mod Expr Expr
  | Smaller Expr Expr
  | Equal Expr Expr
  | NotEqual Expr Expr
  | Bigger Expr Expr
  | BiggerEq Expr Expr
  | SmallerEq Expr Expr
  | Abs Expr
  | Fac Expr
  | Ln Expr
  | Pi Expr
  | ToString Expr
  | Val Float
  | Var Name
  | PString Name
  | Concat Expr Expr
  | ToInt Expr
  | ToFlo Expr
  deriving (Show, Eq, Ord)

-- Ends here

-- The following section of code was modified by kb267 and sb409
-- These are the REPL commands
data Command
  = Set Name Expr -- assign an expression to a variable name
  | Print Expr -- evaluate an expression and print the result
  | Remove Name -- evaluate an expression and print the result
  | Input Name  --promts user input 
  | If Expr Command Command --conditional statement
  | Read Name -- reads a file
  deriving (Show)
-- Ends here

-- The following section of code was written by kb267

-- Overriding the instance for number operations for the handmade type outcome
instance Num Outcome where
  (Integ val) + (Integ val2) = Integ (val + val2)
  (Flo val) + (Flo val2) = Flo (val + val2)
  (Str val) + (Str val2) = Str (val ++ val2)
  (Integ val) + (Flo val2) = Flo (fromIntegral val + val2)
  (Flo val) + (Integ val2) = Flo (val + fromIntegral val2)
  _ + _ = Err "Type incompatible."
  (Integ val) - (Integ val2) = Integ (val - val2)
  (Flo val) - (Flo val2) = Flo (val - val2)
  (Integ val) - (Flo val2) = Flo (fromIntegral val - val2)
  (Flo val) - (Integ val2) = Flo (val - fromIntegral val2)
  _ - _ = Err "Type incompatible."
  (Integ val) * (Integ val2) = Integ (val * val2)
  (Flo val) * (Flo val2) = Flo (val * val2)
  (Integ val) * (Flo val2) = Flo (fromIntegral val * val2)
  (Flo val) * (Integ val2) = Flo (val * fromIntegral val2)
  _ * _ = Err "Type incompatible."

-- Overriding the instance for fractional operations for the handmade type outcome
instance Fractional Outcome where
  (Flo val) / (Flo val2)= Flo ((val) / (val2))
  (Integ val) / (Integ val2)= Flo ((fromIntegral val) / (fromIntegral val2))
  _ / _ = Err "Type incompatible."

-- This function takes in an outcome variable and returns the float value of it
showFlo :: Outcome -> Float
showFlo (Flo x) = x
showFlo (Str x) = read x :: Float
showFlo (Integ x) = fromIntegral x


-- This function takes in an outcome variable and returns the int value of it
showInteg :: Outcome -> Int
showInteg (Integ x) = x
showInteg (Str x) = read x :: Int
showInteg (Flo x) = round x

-- This function takes in an outcome variable and returns the int value of it
showBool :: Outcome -> Bool
showBool (Boolean x) = x

-- Ends here

-- The following function was written by mmc23
-- Takes in an outcome to parse for a Flo if it is a String.
outcomeToFlo :: Outcome -> Outcome
outcomeToFlo (Str x) = case readEither x of
  Right val -> Flo val  -- If it can be parsed, the value is returned
  Left errMsg -> Err errMsg -- Otherwise, an error is returned
outcomeToFlo (Integ x) = Flo (fromIntegral x) -- If the value is an int, it is converted to a float.
outcomeToFlo x = x -- If the value isn't a string or int, no parsing takes place.

-- The following section of code was written by kb267 and was later error handled and converted to binary tree implementation by mmc23
eval ::
  Tree Outcome -> -- Variable name to value mapping
  Expr -> -- Expression to evaluate
  Outcome -- Result (if no errors such as missing variables)

eval vars (Val x) = Flo x -- for values, just give the value directly
eval vars (Add x y) =  eval vars x + eval vars y -- Evaluates each single value and then adds them up
eval vars (Sub x y) =  eval vars x - eval vars y -- Evaluates each single value and then subtracts them by each other
eval vars (Mul x y) =  eval vars x *  eval vars y -- Evaluates each single value and then multiplies them
eval vars (Div x y) =  eval vars x / eval vars y -- Evaluates each single value and then divides them
eval vars (Pwr x y) = flosEval powerFloat (eval vars (ToFlo x)) (eval vars (ToFlo y)) -- Evaluates each single value and then powers up one by another
eval vars (Log x y) = flosEval logBase (eval vars (ToFlo x)) (eval vars (ToFlo y)) -- Evaluates each single value and then takes the log of one by the base of another
eval vars (Mod x y) = flosEval mod' (eval vars (ToFlo x)) (eval vars (ToFlo y)) -- Evaluates each single value and then takes the mod of one by another
eval vars (Smaller x y) = Boolean ((<) (showFlo (eval vars x)) (showFlo (eval vars y))) -- Evaluates each single value and then checks if one is smaller than another
eval vars (Equal x y) = Boolean ((==) (showFlo (eval vars x)) (showFlo (eval vars y))) -- Evaluates each single value and then checks if one is equal to another
eval vars (NotEqual x y) = Boolean ((/=) (showFlo (eval vars x)) (showFlo (eval vars y))) -- Evaluates each single value and then checks if one is not equal to another
eval vars (Bigger x y) = Boolean ((>) (showFlo (eval vars x)) (showFlo (eval vars y))) -- Evaluates each single value and then checks if one is bigger than another
eval vars (SmallerEq x y) = Boolean ((<=) (showFlo (eval vars x)) (showFlo (eval vars y))) -- Evaluates each single value and then checks if one is smaller than another
eval vars (BiggerEq x y) = Boolean ((>=) (showFlo (eval vars x)) (showFlo (eval vars y))) -- Evaluates each single value and then checks if one is bigger than another
eval vars (Abs x) = floEval abs (eval vars (ToFlo x)) -- Evaluates each single value and then takes the mod of one by another
eval vars (Fac x) = floEval (\x -> product [1..x]) (eval vars (ToFlo x)) -- Evaluates the value and then returns the factorial of it
eval vars (Ln x) =  flosEval logBase (eval vars (ToFlo x)) (Flo 2) -- Evaluates the value and then returns the ln of it 
eval vars (Pi x) = floEval (*pi) (eval vars (ToFlo x)) -- Evaluates the value and then returns the multiplication of it with pi
eval vars (ToInt x) = Integ (round (showFlo (eval vars x))) -- Evaluates the value and then returns a rounded version of it in INT
eval vars (ToString x) = Str (show (eval vars x)) -- Evaluates the value and then returns a stringified version of it
eval vars (PString x) = Str x -- Evaluates the value and then returns a string of it
eval vars (Concat x y) = Str (show(eval vars x) ++ show(eval vars y)) -- Evaluates each single value and then returns a concatenated version of it
eval vars (ToFlo x) = outcomeToFlo (eval vars x) -- Evaluates each single value and then returns a concatenated version of it
eval vars (Var x) = case varLookup x vars of -- Evaluates the value and looks in the list of variables for it
  Right value -> value -- If found a variable is returned
  Left errorMsg -> Err errorMsg -- If not an error is returned

-- Ends here

-- The following section of code was written by mmc23

-- Evaluates a function taking in 2 floats and returning a Float and returns an appropriate Outcome with error handling included
flosEval :: (Float -> Float -> Float) -> -- Expression to evaluate
  Outcome ->
  Outcome ->
  Outcome -- Result (if no errors such as missing variables)
flosEval _ (Err errMsg) _ = Err "Type incompatible."
flosEval _ _ (Err errMsg) = Err "Type incompatible"
flosEval func (Flo x) (Flo y) = Flo (func x y) -- Evaluates each single value and then powers up one by another


-- Evaluates a function taking in a Float and returning a Float and returns an appropriate Outcome with error handling included
floEval :: (Float -> Float) -> -- Expression to evaluate
  Outcome ->
  Outcome -- Result (if no errors such as missing variables)

floEval _ (Err errMsg) = Err "Type incompatible."
floEval func (Flo x) = Flo (func x) -- Evaluates each single value and then powers up one by another

-- Ends here

-- The following section of code was written by mmc23
-- Checks whether the result of an eval call was successful or not. Returns either an appropriate error or an updated Tree with the new value assigned to the correct variable.
safeEval :: Name -> Tree Outcome -> Expr -> Either ErrorCase (Tree Outcome)
safeEval var tree e = case eval tree e of
  Err errorMsg -> Left errorMsg
  val -> Right (treeAdd var val tree)
-- Ends here

digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'

-- The following section of code was written by kb267

-- Method which takes in a number and a list of commands 
-- and repeatedly adds them to the back of eachother
-- To mock looping functionality
pRepeat :: Int -> [Command] -> [Command]
pRepeat 0 commands = []
pRepeat 1 commands = commands
pRepeat n commands = commands ++ pRepeat (n-1) commands

-- A parser for a list of commands
-- Allows the implementation of loops
-- And multiple commands
pCommands :: Parser [Command]
pCommands =
      do
      space
      string "repeat"
      space
      rounds <- nat
      space
      symbol "{"
      space
      e0 <- pCommands
      space
      symbol "}"
      let eFin = pRepeat rounds e0
      return eFin
      |||
      do
      space
      e1 <- pCommand
      do
        space
        char ';'
        space
        e2 <- pCommand
        space
        let tempResult = e1: [e2]
        finalResult <- pCommandsRecurse tempResult
        space
        return finalResult
        |||
        return [e1]

-- A command parser which takes in input from command line
-- And passes it to the correct expression parser
pCommand :: Parser Command
pCommand =
  do
    space
    t <- many letter
    space
    char '='
    space
    string "input"
    return (Input t)
  |||
    do
    space
    t <- many letter
    space
    char '='
    space
    e <- pExpr
    space
    return (Set t e)
    ||| do
      space
      string "print"
      space
      e <- pExpr
      space
      return (Print e)
    ||| do
      space
      string "rem"
      space
      t <- many letter
      space
      return (Remove t)
      |||
        do
        space
        string "if"
        space
        op <- pExpr
        space
        string "then"
        space
        e1 <- pCommand
        space
        string "else"
        space
        e2 <- pCommand
        space
        return (If op e1 e2)
      |||
        do
        space
        string "read"
        space
        fname <- allInput
        space
        return (Read fname)

-- Ends here

-- The following section of code was witten by sb409
toExpr :: String -> Expr
toExpr inp = do
  PString inp

-- Ends here

-- The following section of code was written by kb267

-- An expression parser which parses a term first
-- And then awaits one of the  + - or ++ symbols to do the specific operation on input
pExpr :: Parser Expr
pExpr = do t <- pTerm
           space
           do char '+'
              space
              e <- pExpr
              space
              return (Add t e)
            ||| do
                   char '-'
                   space
                   e <- pTerm
                   let tempResult = Sub t e
                   finalResult <- pSubtractRecurse tempResult
                   space
                   return finalResult
            |||do
              string "++"
              space
              e2 <- pExpr
              space
              return (Concat (ToString t) (ToString e2))
            ||| return t

-- The addition of commands to the end of each other 
-- The command after ; gets added to the list of commands before ;
-- If no further command is found the current list is returned
pCommandsRecurse :: [Command] -> Parser [Command]
pCommandsRecurse tempResult = do
             char ';'
             e2 <- pCommand
             space
             let finalResult = tempResult ++ [e2]
             pCommandsRecurse finalResult
             ||| do
             return tempResult

-- The function which applies input precedency for operation -
-- This was made to oppose the right to left (inside out) evaluation of expressions
-- Now what happens is that a term gets evaluated and then gets subtracted from the term before - and the parser moves on to the next term
-- With the result of the current operation being stored as the term before -
pSubtractRecurse :: Expr -> Parser Expr
pSubtractRecurse tempResult =  do
             char '-'
             e2 <- pTerm
             space
             let finalResult = Sub tempResult e2
             pSubtractRecurse finalResult
             ||| do
             char '+'
             space
             e <- pExpr
             let finalResult = Add tempResult e
             return finalResult
             ||| do
             return tempResult

-- The function which applies input precedency for operation /
-- This was made to oppose the right to left (inside out) evaluation of expressions
-- Now what happens is that a term gets evaluated and then the term before / is divided by the term after it and the parser moves on to the next term
-- With the result of the current operation being stored as the term before /
pDivideRecurse :: Expr -> Parser Expr
pDivideRecurse tempResult =  do
             char '/'
             e2 <- pTerm
             space
             let finalResult = Div tempResult e2
             pDivideRecurse finalResult
             ||| do
             char '+'
             space
             e <- pExpr
             let finalResult = Add tempResult e
             return finalResult
             ||| do
             char '*'
             space
             e <- pExpr
             let finalResult = Mul tempResult e
             return finalResult
             ||| do
             return tempResult

-- A factor parser (Lowest level of evaluation) which proceeds to evaluate the given expressions fir the given operations
-- These have the highest precedency in the language
pFactor :: Parser Expr
pFactor = do string "toString"
             space
             e <- pExpr
             space
             return (ToString e)
            |||do string "mod"
                  space
                  e <- pExpr
                  space
                  e2 <- pExpr
                  space
                  return (Mod e e2)
            |||do
                string "toInt"
                space
                e <- pExpr
                space
                return (ToInt e)
            |||do string "abs"
                  space
                  e <- pExpr
                  space
                  return (Abs e)
            |||do string "fac"
                  space
                  e <- pExpr
                  space
                  return (Fac e)
            |||do string "log"
                  space
                  e <- pExpr
                  space
                  e2 <- pExpr
                  space
                  return (Log e e2)
            |||do string "ln"
                  space
                  e <- pExpr
                  space
                  return (Ln e)
            |||do string "pi"
                  space
                  e <- pExpr
                  space
                  return (Pi e)
            |||do
                symbol "\""
                e1t <-  allInput
                let e1 = PString e1t
                symbol "\""
                return e1
            |||
              value
                ||| do char '('
                       e <- pExpr
                       char ')'
                       return e

-- A term parser which takes in a factor and tries to match one of the following commands to apply the operation on
-- If not matched the factor is returned
-- These have higher precedency than + - & ++ just like regular maths
pTerm :: Parser Expr
pTerm = do f <- pFactor
           space
           do char '*'
              space
              t <- pTerm
              space
              return (Mul f t)
            ||| do
                   char '/'
                   space
                   e <- pFactor
                   let tempResult = Div f e
                   finalResult <- pDivideRecurse tempResult
                   space
                   return finalResult
            ||| do char '^'
                   space
                   t <- pTerm
                   space
                   return (Pwr f t)
            ||| do string "=="
                   space
                   t <- pTerm
                   space
                   return (Equal f t)
            ||| do string "!="
                   space
                   t <- pTerm
                   space
                   return (NotEqual f t)
            ||| do string "<"
                   space
                   t <- pTerm
                   space
                   return (Smaller f t)
            ||| do string ">"
                   space
                   t <- pTerm
                   space
                   return (Bigger f t)
            ||| do string "<="
                   space
                   t <- pTerm
                   space
                   return (SmallerEq f t)
            ||| do string ">="
                   space
                   t <- pTerm
                   space
                   return (BiggerEq f t)
                 ||| return f

-- A variable parser
-- Matches the command line input with the identifier parse and returns a variable
var :: Parser Expr
var = do
  var <- identifier
  return (Var var)

-- A negation parser
-- Used as a helper tool for variable negation
neg :: Expr
neg = Val (-1)

-- Negative variable parser
-- Used to match negative variables such as "-x" 
-- and returns the result of the multiplication of -1 by the value of te variable 
nVar :: Parser Expr
nVar = do
  symbol "-"
  nVar <- identifier
  return (Mul (Var nVar) neg)

-- value parser
-- Used to match any entry numbers so that both floats nd its get accepted as values
val :: Parser Expr
val = do
  i <- numb
  return (Val i)

-- A value parser which matches either a value or a variable 
value :: Parser Expr
value = val ||| var ||| nVar

-- Ends here