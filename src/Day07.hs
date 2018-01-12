module Day07 where
import Lib (fileToLines, apply)
import Data.List ((!!), findIndices, findIndex)

data Node = Node Name [Name] deriving (Eq, Show)
type Name = String

day07_1 :: IO String
day07_1 = apply (getRootNode . createNodes) contents
  where
    contents = fileToLines path
    path = "/mnt/c/Users/Robin/adventofcode2017/inputs/day07.txt"

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

createNodes :: [String] -> [Node]
createNodes ss = map parseNode ss

parseNode :: String -> Node
parseNode s = Node (findName tokens) (findChildren tokens)
  where
    tokens = words s

findName :: [String] -> String
findName (n:_) = n

findChildren :: [String] -> [String]
findChildren cs
  | length cs > 2 = map (filter (/= ',')) (drop 3 cs)
  | otherwise = []

getName :: Node -> Name
getName (Node name _) = name

getChildren :: Node -> [Name]
getChildren (Node _ children) = children

newNode :: Name -> [Name] -> Node
newNode name children = Node name children

addChild :: Node -> Name -> Node
addChild (Node name children) child = Node name (child:children)

isLeaf :: Node -> Bool
isLeaf (Node _ []) = True
isLeaf _ = False

--------------------------------------------------------------------------------

