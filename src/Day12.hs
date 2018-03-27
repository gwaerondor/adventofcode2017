module Day12 where
import Lib (fileToLines, apply)
import Data.String (words)
import Data.List (nub, (\\))

data Pipe = Pipe Int [Int] deriving (Eq, Show)

day12_1 :: IO String
day12_1 = apply countSizeOfGroupZero contents
  where
    contents = fileToLines path
    path = "inputs/day12.txt"

countSizeOfGroupZero :: [String] -> Int
countSizeOfGroupZero lines = length $ findAllConnectedPipes 0 pipes
  where
    pipes = parsePipes lines

parsePipes :: [String] -> [Pipe]
parsePipes lines = map parsePipe lines 

parsePipe :: String -> Pipe
parsePipe line = Pipe from to
  where
    w = words $ filter (/= ',') line
    from = read (w !! 0)
    to = map read $ drop 2 w

findAllConnectedPipes :: Int -> [Pipe] -> [Int]
findAllConnectedPipes index pipes = nub $ index:(facp index pipes [])

facp :: Int -> [Pipe] -> [Int] -> [Int]
facp ix ps traversed
  | found == [] = []
  | otherwise = found ++ moreChildren
  where
    found = (findChildren ix ps) \\ traversed
    newTraversed = (ix:traversed)
    moreChildren = concat $ map (\ix -> facp ix ps newTraversed) found
                   
findChildren ix ((Pipe i cs):ps)
  | ix == i = cs
  | otherwise = findChildren ix ps

getFrom :: Pipe -> Int
getFrom (Pipe from _) = from

--------------------------------------------------------------------------------
day12_2 :: IO String
day12_2 = apply countNumberOfGroups contents
  where
    contents = fileToLines path
    path = "inputs/day12.txt"

countNumberOfGroups lines = cnog pipes
  where
    pipes = parsePipes lines

cnog :: [Pipe] -> Int
cnog [] = 0
cnog (p:ps) = 1 + (cnog filtered)
  where
    firstIndex = getFrom p
    allConnected = findAllConnectedPipes firstIndex (p:ps)
    filtered = filterPipes (p:ps) allConnected

filterPipes :: [Pipe] -> [Int] -> [Pipe]
filterPipes [] _ = []
filterPipes (p:ps) exclude
  | (getFrom p) `elem` exclude = filterPipes ps exclude
  | otherwise = p:(filterPipes ps exclude)
