module Day07 where
import Lib (fileToLines, apply)
import Data.List ((!!), findIndices, findIndex)
import Data.Char (isNumber)

data Node = Node Name Weight [Name] deriving (Eq, Show)
type Name = String
type Weight = Int

day07_1 :: IO String
day07_1 = apply (getRootNode . createNodes) contents
  where
    contents = fileToLines path
    path = "inputs/day07.txt"

createNodes :: [String] -> [Node]
createNodes ss = map parseNode ss

getRootNode :: [Node] -> String
getRootNode nodes = getRootNode' nodes 0

getRootNode' :: [Node] -> Int -> String
getRootNode' nodes ix = case getParentIndex (nodes !! ix) nodes of
                          Nothing -> getName (nodes !! ix)
                          Just n -> getRootNode' nodes n

getParentIndex :: Node -> [Node] -> Maybe Int
getParentIndex n ns = findIndex (`hasChild` (getName n)) ns

hasChild :: Node -> Name -> Bool
hasChild n c = any (== c) (getChildren n)

parseNode :: String -> Node
parseNode s = Node (findName tokens) (findWeight tokens) (findChildren tokens)
  where
    tokens = words s

findName :: [String] -> String
findName (n:_) = n

findChildren :: [String] -> [String]
findChildren cs
  | length cs > 2 = map (filter (/= ',')) (drop 3 cs)
  | otherwise = []

findWeight :: [String] -> Int
findWeight cs = read $ filter isNumber $ cs !! 1

getName :: Node -> Name
getName (Node name _ _) = name

getChildren :: Node -> [Name]
getChildren (Node _ _ children) = children

newNode :: Name -> Int -> [Name] -> Node
newNode name weight children = Node name weight children

addChild :: Node -> Name -> Node
addChild (Node name weight children) child = Node name weight (child:children)

isLeaf :: Node -> Bool
isLeaf (Node _ _ []) = True
isLeaf _ = False

--------------------------------------------------------------------------------
day07_2 :: IO String
day07_2 = apply something contents
  where
    contents = fileToLines path
    path = "inputs/day07.txt"

something :: [String] -> String
something _ = "Not done"

getNodeWeight :: Node -> Int
getNodeWeight (Node _ weight _) = weight
