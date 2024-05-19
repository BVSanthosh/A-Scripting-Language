module Tree where
import Outcome

-- The following section of code was written by mmc23
type Name = String

-- Variables are stored as a Tree structure. Nodes contain a key and a value.
data Tree a = Leaf
          | Node (Tree a) Name a (Tree a)
            deriving (Show)

-- Parses a tree and returns an array of its keys
showTree :: Tree a -> Int -> [[Char]]
showTree Leaf _ = []
showTree (Node left key val right) n = [key] ++ showTree left (n+1) ++ showTree right (n+1)

-- Searches a variable Tree, returning the corresponding value if the variable exists and an error if not
treeSearch :: Name -> Tree Outcome -> Outcome
treeSearch key Leaf = Err "Value not found."
treeSearch key (Node left tKey val right) = case compare key tKey of
       EQ -> val
       LT -> treeSearch key left
       GT -> treeSearch key right


-- Runs the search method but returns the value with an either case attached to allow for further error handling
varLookup :: Name -> Tree Outcome -> Either ErrorCase Outcome
varLookup x vars = case treeSearch x vars of
  Err errMsg -> Left errMsg
  val -> Right val


-- Adds a node to a variable Tree or updates the value if one with the corresponding Key already exists
treeAdd :: Name -> Outcome -> Tree Outcome -> Tree Outcome
treeAdd key val Leaf = Node Leaf key val Leaf
treeAdd key val (Node left tKey tVal right) = case compare key tKey of
      EQ -> Node left tKey val right
      LT -> Node (treeAdd key val left) tKey tVal right
      GT -> Node left tKey tVal (treeAdd key val right)

-- Removes a node from the tree, updating any children accordingly
treeRemove :: String -> Tree Outcome -> Tree Outcome
treeRemove key Leaf = Leaf
treeRemove key (Node left tKey tVal right) = case compare key tKey of
      EQ -> removeNode (Node left tKey tVal right)
      LT -> Node (treeRemove key left) tKey tVal right
      GT -> Node left tKey tVal (treeRemove key right)

-- Handles the updating of children during the removal of a Node
removeNode :: Tree Outcome -> Tree Outcome
removeNode (Node Leaf key val Leaf) = Leaf
removeNode (Node Leaf key val right) = right
removeNode (Node left key val Leaf) = left
removeNode (Node (Node childLeft newKey newVal childRight) key val right) = Node (removeNode (Node childLeft newKey newVal childRight)) newKey newVal right

-- Outputs all values currently stored in the tree along with their keys
printTree :: Tree Outcome -> IO ()
printTree Leaf = return ()
printTree (Node left key val right) = do
      printTree left
      printTree right
      putStrLn (key ++ " = " ++ show val)

-- Ends here