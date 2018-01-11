module Day07 where
import Lib (fileToLines, apply)

data Tree = Tree Name [Tree] deriving (Eq, Show)
type Name = String

day07_1 :: IO String
day07_1 = apply (getRootNode . createTree) contents
  where
    contents = fileToLines path
    path = "/mnt/c/Users/Robin/adventofcode2017/inputs/day07.txt"

getRootNode :: Tree -> String
getRootNode tree = getName tree

createTree :: [String] -> Tree
createTree _ = newTree "hello" []

getName :: Tree -> Name
getName (Tree name _) = name

getChildren :: Tree -> [Tree]
getChildren (Tree _ children) = children

newTree :: Name -> [Tree] -> Tree
newTree name children = Tree name children

addChild :: Tree -> Tree -> Tree
addChild (Tree name children) child = Tree name (child:children)

isLeaf :: Tree -> Bool
isLeaf (Tree _ []) = True
isLeaf _ = False
