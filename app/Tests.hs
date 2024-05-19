{-# LANGUAGE TemplateHaskell #-}
module Main where
import Outcome
import Parsing
import Tree
import REPL
import Test.QuickCheck
import Data.Maybe (isJust)
import Control.Monad.IO.Class
import Test.QuickCheck.Monadic
import Test.QuickCheck.All
import System.Directory
import Expr
import Data.Fixed
import GHC.Float

-- The following section of code was written by mmc23

-- Iterates through a Tree and adds its keys to a list. Created separate to showTree so that it returns [String] instead of [[Char]]
treeKeys :: Tree a -> [String] -> [String]
treeKeys Leaf xs = xs
treeKeys (Node left key val right) xs = treeKeys right (treeKeys left (xs ++ [key]))

-- Takes a Tree and picks a random key from it
generateRandomKey :: Tree a -> Gen String
generateRandomKey tree = elements (treeKeys tree [])

-- Helper method for testing the arbitrary tree generator. Returns the tree and a random key, value pair.
genTest :: Gen (Tree Outcome, Name, Outcome)
genTest = do
    tree <- arbitrary
    val1 <- generateRandomKey tree
    return (tree, val1, treeSearch val1 tree)


-- Eval case tests use the Tree generator to create a random dataset and then uses generateRandomKey to pick random values 
-- The result of the eval call is compared to the intended operation on the same values to see if it was successful.
-- Any cases that can have a NaN result have their results stored so a proper NaN comparison can be run with the helper method.

-- Tests the Add case for eval on random values
prop_eval_add :: Gen Property
prop_eval_add = do
    tree <- arbitrary
    val1 <- generateRandomKey tree
    val2 <- generateRandomKey tree
    return (eval tree (Add (Var val1) (Var val2)) === treeSearch val1 tree + treeSearch val2 tree)


-- Tests the Sub case for eval on random values
prop_eval_sub :: Gen Property
prop_eval_sub = do
    tree <- arbitrary
    val1 <- generateRandomKey tree
    val2 <- generateRandomKey tree
    return (eval tree (Sub (Var val1) (Var val2)) === treeSearch val1 tree - treeSearch val2 tree)


-- Tests the Mul case for eval on random values
prop_eval_mul :: Gen Property
prop_eval_mul = do
    tree <- arbitrary
    val1 <- generateRandomKey tree
    val2 <- generateRandomKey tree
    return (eval tree (Mul (Var val1) (Var val2)) === treeSearch val1 tree * treeSearch val2 tree)


-- Tests the Div case for eval on random values.
prop_eval_div :: Gen Property
prop_eval_div = do
    tree <- arbitrary
    val1 <- generateRandomKey tree
    val2 <- generateRandomKey tree
    let res1 = eval tree (Div (Var val1) (Var val2))
    let res2 = treeSearch val1 tree / treeSearch val2 tree
    return ((res1 == res2 || handleNanCase res1 res2) === True)


-- Tests the Pwr case for eval on random values
prop_eval_pwr :: Gen Property
prop_eval_pwr = do
    tree <- arbitrary
    val1 <- generateRandomKey tree
    val2 <- generateRandomKey tree
    return (eval tree (Pwr (Var val1) (Var val2)) === flosEval powerFloat (outcomeToFlo (treeSearch val1 tree)) (outcomeToFlo (treeSearch val2 tree)))


-- Tests the Log case for eval on random values
prop_eval_log :: Gen Property
prop_eval_log = do
    tree <- arbitrary
    val1 <- generateRandomKey tree
    val2 <- generateRandomKey tree
    let res1 = eval tree (Log (Var val1) (Var val2))
    let res2 = flosEval logBase (outcomeToFlo (treeSearch val1 tree)) (outcomeToFlo (treeSearch val2 tree))
    return ((res1 == res2 || handleNanCase res1 res2) === True)


-- Tests the Mod case for eval on random values
prop_eval_mod :: Gen Property
prop_eval_mod = do
    tree <- arbitrary
    val1 <- generateRandomKey tree
    val2 <- generateRandomKey tree
    let res1 = eval tree (Mod (Var val1) (Var val2))
    let res2 = flosEval mod' (outcomeToFlo (treeSearch val1 tree)) (outcomeToFlo (treeSearch val2 tree))
    return ((showFlo (treeSearch val2 tree) == 0 || res1 == res2) === True)


-- Tests the Smaller case for eval on random values
prop_eval_smaller :: Gen Property
prop_eval_smaller = do
    tree <- arbitrary
    val1 <- generateRandomKey tree
    val2 <- generateRandomKey tree
    return (eval tree (Smaller (Var val1) (Var val2)) === Boolean (treeSearch val1 tree < treeSearch val2 tree))


-- Tests the Equal case for eval on random values
prop_eval_equal :: Gen Property
prop_eval_equal = do
    tree <- arbitrary
    val1 <- generateRandomKey tree
    val2 <- generateRandomKey tree
    return (eval tree (Equal (Var val1) (Var val2)) === Boolean (treeSearch val1 tree == treeSearch val2 tree))


-- Tests the NotEqual case for eval on random values
prop_eval_not_equal :: Gen Property
prop_eval_not_equal = do
    tree <- arbitrary
    val1 <- generateRandomKey tree
    val2 <- generateRandomKey tree
    return (eval tree (NotEqual (Var val1) (Var val2)) === Boolean (treeSearch val1 tree /= treeSearch val2 tree))


-- Tests the Bigger case for eval on random values
prop_eval_bigger :: Gen Property
prop_eval_bigger = do
    tree <- arbitrary
    val1 <- generateRandomKey tree
    val2 <- generateRandomKey tree
    return (eval tree (Bigger (Var val1) (Var val2)) === Boolean (treeSearch val1 tree > treeSearch val2 tree))


-- Tests the SmallerEq case for eval on random values
prop_eval_smaller_eq :: Gen Property
prop_eval_smaller_eq = do
    tree <- arbitrary
    val1 <- generateRandomKey tree
    val2 <- generateRandomKey tree
    return (eval tree (SmallerEq (Var val1) (Var val2)) === Boolean (treeSearch val1 tree <= treeSearch val2 tree))


-- Tests the BiggerEq case for eval on random values
prop_eval_bigger_eq :: Gen Property
prop_eval_bigger_eq = do
    tree <- arbitrary
    val1 <- generateRandomKey tree
    val2 <- generateRandomKey tree
    return (eval tree (BiggerEq (Var val1) (Var val2)) === Boolean (treeSearch val1 tree >= treeSearch val2 tree))


-- Tests the Abs case for eval on random values
prop_eval_abs :: Gen Property
prop_eval_abs = do
    tree <- arbitrary
    val1 <- generateRandomKey tree
    return (eval tree (Abs (Var val1)) === floEval abs (outcomeToFlo (treeSearch val1 tree)))


-- Tests the Fac case for eval on random values
prop_eval_fact :: Gen Property
prop_eval_fact = do
    tree <- arbitrary
    val1 <- generateRandomKey tree
    return (eval tree (Fac (Var val1)) === floEval (\x -> product [1..x]) (outcomeToFlo (treeSearch val1 tree)))


-- Tests the Ln case for eval on random values
prop_eval_ln :: Gen Property
prop_eval_ln = do
    tree <- arbitrary
    val1 <- generateRandomKey tree
    let res1 = eval tree (Ln (Var val1))
    let res2 = flosEval logBase (outcomeToFlo (treeSearch val1 tree)) (Flo 2)
    return ((res1 == res2 || handleNanCase res1 res2) === True)


-- Tests the Pi case for eval on random values
prop_eval_pi :: Gen Property
prop_eval_pi = do
    tree <- arbitrary
    val1 <- generateRandomKey tree
    return (eval tree (Pi (Var val1)) === floEval (* pi) (outcomeToFlo (treeSearch val1 tree)))


-- Tests the ToInt case for eval on random values
prop_eval_to_int :: Gen Property
prop_eval_to_int = do
    tree <- arbitrary
    val1 <- generateRandomKey tree
    return (eval tree (ToInt (Var val1)) === Integ (round (showFlo (treeSearch val1 tree))))


-- Tests the ToString case for eval on random values
prop_eval_to_string :: Gen Property
prop_eval_to_string = do
    tree <- arbitrary
    val1 <- generateRandomKey tree
    return (eval tree (ToString (Var val1)) === Str (show (treeSearch val1 tree)))


-- Tests the Concat case for eval on random values
prop_eval_concat :: Gen Property
prop_eval_concat = do
    tree <- arbitrary
    val1 <- generateRandomKey tree
    val2 <- generateRandomKey tree
    return (eval tree (Concat (Var val1) (Var val2)) === Str (show (treeSearch val1 tree) ++ show (treeSearch val2 tree)))



-- A Helper method used to compare results that can return NaN. This is done as directly comparing two NaNs always returns False.
handleNanCase :: Outcome -> Outcome -> Bool
handleNanCase (Flo x) (Flo y) = isNaN x && isNaN y


-- Defining arbitrary for a Tree. 
instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = arbitraryLengthTree 80 6 -- A base key value of 80 and a length of 6 have been arbitrarily chosen as values that guarantee a valid data set in all cases.


-- Defining arbitrary for an Outcome. This always uses an Integer value as they can be used in all tests.
instance Arbitrary Outcome where
    arbitrary = do
        Integ <$> arbitrary


-- The generator method for an Arbitrary Length Tree.
-- Takes a base value for the Key and the length of the tree to generate recursively.
arbitraryLengthTree :: Arbitrary a => Int -> Int -> Gen (Tree a)
arbitraryLengthTree keyBase 0 = return Leaf -- Base case for length 0 as a Leaf is a Tree with 0 Nodes.
arbitraryLengthTree keyBase len = do
    let halfLen = len `div` 2 -- Finding half of the input length for further use.
    let key = return (toEnum keyBase) -- Uses the base key value for the current node
    val <- arbitrary -- Generating an arbitrary value for the node
    left <- arbitraryLengthTree (keyBase - halfLen) halfLen -- Recursively calls the method with a new keyBase and appropriate length for the left branch of the node.
    right <- arbitraryLengthTree (keyBase + halfLen) halfLen -- Does the same as the above for the right branch of the node.
    return (Node left key val right) -- Returns the completed Node along with its branches


-- Runs all of the property tests
return []
check :: IO Bool
check = $quickCheckAll


-- Calls the method to run the property tests
main :: IO Bool
main = check

-- Ends here
