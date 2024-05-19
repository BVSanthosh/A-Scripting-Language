module Main where

import Parsing
import Expr
import REPL

main :: IO ((), LState)
main = repl
