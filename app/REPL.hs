{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module REPL where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Text
import Expr
import Parsing
import System.Console.Haskeline
import System.Console.Haskeline.History
import System.Exit
import System.IO
import Data.Either
import Tree
import Outcome

data LState = LState {vars :: Tree Outcome}
  deriving (Show)

initLState :: LState
initLState = LState Leaf

-- The following section of code was written by kb267 and was later converted to binary tree implementation by mmc23
-- Return a new set of variables with the given name removed
dropVar :: Name -> Tree Outcome -> (String, Tree Outcome)
dropVar var st = case varLookup var st of
  Right cValue -> (var ++ " removed", treeRemove var st)
  Left errorMsg -> (errorMsg, st)
-- Ends here


-- The following section of code was written by kb267 and modified by mmc23 to add error handling with Either
-- Processes the command returned by the expression parser
-- Then evaluates it
-- And finally updates the state of the program with it
process :: LState -> Command -> IO LState
process st (Set var e) =
  do
    -- If there is an error in the eval, it is displayed and the state does not change
    case safeEval var (vars st) e of
      Left errMsg -> do
        putStrLn(errMsg)
        return st
      Right st' -> return (LState st')
    -- st' should include the variable set to the result of evaluating e
process st (Print e) =
  do
    let value = eval (vars st) e
    putStrLn (show value)
    let st' = LState (vars st)
    -- Print the result of evaluation
    return st'
process st (Remove e) =
  do
    let result = dropVar e (vars st)
    let st' = LState (snd result)
    print (fst result)
    return st'
-- Ends here
-- The following section of code was written by sb409
process st (Input var) =
  do
    -- In this case it takes in an user input and processes the outcome of it again
    input <- getLine
    -- Converts the input to an expression
    let typeConverted = toExpr input
    let cmd = Set var typeConverted
    process st cmd
-- Section ends here
-- The following section of code was written by sb409 with further inputs and enhancementts from kb267
process st (If op e1 e2) =
  do
    let value = eval (vars st) op
    if showBool(value) == True then
      do
        st' <- (process st e1)
        return st'
    else
      do
        st' <- (process st e2)
        return st'
-- Section ends here

-- The following section of code was written by kb267
-- Takes in a list of strings and concatenates all elements of it into one string adding ; between them
-- Useful for parsing and processing files
concatLines :: [String] -> String
concatLines [] = []
concatLines (x:xs) = x ++ ";" ++ concatLines xs
-- Section ends here

-- readfile Error handler
loadError :: IOError -> IO String
loadError e =
  do
    putStrLn "Cannot load the given file! Please try again\nopenFile: does not exist (No such file or directory)"
    return "" 

-- The following section of code was written by sb409 with further inputs and enhancementts from kb267
fileHandler :: p -> Command -> IO [Char]
fileHandler st (Read var) =
  do
    linesOfCode <- readFile var `catch` loadError
    let codeList = lines linesOfCode
    let input = slice 0 ((length (concatLines codeList))-1) (concatLines codeList)
    return input
-- Section ends here

-- The following section of code was written by kb267
-- Auto complete

-- The following section of code was inspired by the following articles:
-- amindfv (2011). Haskell (haskeline) word completion. [online] Stack Overflow. Available at: https://stackoverflow.com/questions/6147201/haskell-haskeline-word-completion [Accessed 24 Mar. 2022].
-- & 
-- reddit. (2013). r/haskell - Haskeline woes. [online] Available at: https://www.reddit.com/r/haskell/comments/1os0yq/haskeline_woes/ [Accessed 24 Mar. 2022].

-- This function searches the variables list in stateT and checks if the current input in CLI is prefix of any of the variables in there
findCompletion :: String -> StateT LState IO [Completion]
findCompletion s = get >>= \ns -> return $ map simpleCompletion $ filter (s `isPrefixOf`) (showTree (vars ns) 0)

-- >>= \ns -> return $ map simpleCompletion $ filter (s `isPrefixOf`) (ns)

-- The modified settings for the haskeline function
-- Which gives it a history file to store the history commands in it and takes in a autocomplete function to automatically complete variable names 
hlSettings :: Settings (StateT LState IO)
hlSettings = setComplete (completeWord Nothing " \t" findCompletion) defaultSettings {historyFile = Just "commands.hist", autoAddHistory = True}

-- section ends here

-- The repl function which runs a stateT (Storing the variable names)
-- And looping the loop function for the program to flow
repl :: IO ((), LState)
repl = do runStateT (runInputT hlSettings loop) initLState

-- This function trims a string
-- Used to remove the final ; when reading a file
slice :: Int -> Int -> [a] -> [a]
slice i j s = take (j - i) (drop i s)

-- The following section of code was inspired by the following article:
-- Programming-idioms.org. (2022). Find substring position, in Haskell. [online] Available at: https://programming-idioms.org/idiom/62/find-substring-position/968/haskell [Accessed 24 Mar. 2022].

-- Section ends here

-- This function processes each command in the list of commands passed on to it
-- This is used to evaluate multiple commands seperated by ; 
processList :: LState -> [Command] -> IO LState
processList st [] = do return st
processList st (x:xs) = do
                          cSt <- process st x
                          processList cSt xs

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'loop' when done, so the system loops.
loop :: InputT (StateT LState IO) ()
loop = do
  -- An input is taken from the user in CLI
  minput <- getInputLine "> "
  case minput of
    Nothing -> return ()
    -- If quit is inputted the program exis
    Just "quit" -> return ()
    -- In all the other cases the input is parsed
    Just input -> do
      case parse pCommands input of
        [(cmd, "")] ->
          -- Must parse entire input
          do
            -- The following section of code was inspired by the following article:
            -- reddit. (2013). r/haskell - Haskeline woes. [online] Available at: https://www.reddit.com/r/haskell/comments/1os0yq/haskeline_woes/ [Accessed 24 Mar. 2022].
            st <- lift get
            -- Section ends here
            -- The base code of following section of code was written by sb409 
            -- And was later implemented by kb267
            if ".txt" `isInfixOf` input
                then
                  do
                    cmds <- liftIO (fileHandler st (head cmd))
                    cmd' <- do
                      case parse pCommands cmds of
                        [(cmder,"")] ->
                          do
                            return cmder
                        _ ->
                          do
                            outputStrLn "Bad parse"
                            return []
                    st' <- liftIO (processList st cmd')
                    lift $ put st'
                    loop
                else
                  -- Code ends here
                  do
                    -- Then proceeds to process the list of lscommands retunred by the parser
                    st' <- liftIO (processList st cmd)
                    lift $ put st'
                    -- Finally it loops with the new state
                    loop
        _ -> do
          -- If no pattern is matched by the parser an error is outputted
          outputStrLn "Parse error"
          -- Finally it loops with the current state
          loop

-- Ends here
