module Day07 where
import Lib (fileToLines, apply)
import Data.List ((!!))
import Data.Char (isNumber)
import Data.Tree (Tree (..))

data Program = Program Name Weight deriving (Eq, Show)
type Name = String
type Weight = Int

day07_1 :: IO String
day07_1 = apply findRoot contents
  where
    contents = fileToLines path
    path = "inputs/day07.txt"

findRoot :: [String] -> String
findRoot c = fr c c

fr :: [String] -> [String] -> String
fr (current:more) contents
  | isRoot = findName $ words current
  | otherwise = fr more contents
  where
    isRoot = not (hasParent current contents)

hasParent :: String -> [String] -> Bool
hasParent current contents = name `elem` allChildren
  where
    name = findName tokens
    tokens = words current
    allChildren = concat $ map (findChildren . words) contents

findName :: [String] -> String
findName (n:_) = n

findChildren :: [String] -> [String]
findChildren cs
  | length cs > 2 = map (filter (/= ',')) (drop 3 cs)
  | otherwise = []

findWeight :: [String] -> Int
findWeight cs = read $ filter isNumber $ cs !! 1

--------------------------------------------------------------------------------
day07_2 :: IO String
day07_2 = apply (show . findCorrectWeight) contents
  where
    contents = fileToLines path
    path = "inputs/day07.txt"

findCorrectWeight :: [String] -> Int
findCorrectWeight contents = head $
                             map getWeight $
                             findInbalancedLayer $
                             makeTree (map words contents) root
  where
    root = findRoot contents

makeTree :: [[String]] -> String -> Tree Program
makeTree contents label = Node (Program label weight) children
  where
    currentContents = findRow label contents
    weight = findWeight currentContents
    childNames = findChildren currentContents
    children = map (makeTree contents) childNames
                     
findRow :: Name -> [[String]] -> [String]
findRow label (c:cs)
  | head c == label = c
  | otherwise = findRow label cs

findInbalancedLayer :: Tree Program -> [Tree Program]
findInbalancedLayer (Node _ children) = children -- Not correct, unfinished implementation!

getWeight :: Tree Program -> Int
getWeight (Node (Program _ weight) []) = weight
getWeight (Node (Program _ weight) children) = weight + (sum $ map getWeight children)
