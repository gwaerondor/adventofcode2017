module Day13 where
import Lib (fileToLines, apply)
import Data.String (words)

type Layer = Int
type Depth = Int
type Position = Int
data Direction = Up | Down deriving (Show)
data Scanner = Scanner Layer Depth Position Direction deriving (Show)

day13_1 = apply countSeverity contents
  where
    contents = fileToLines path
    path = "inputs/day13.txt"

parseScanners :: [String] -> [Scanner]
parseScanners lines = map parseScanner lines

parseScanner :: String -> Scanner
parseScanner line = Scanner layer depth 0 Down
  where
    w = words line
    layer = read $ filter (/= ':') $ w !! 0
    depth = read $ w !! 1

countSeverity :: [String] -> Int
countSeverity contents = countSeverityStepwise scanners
  where
    scanners = parseScanners contents

countSeverityStepwise :: [Scanner] -> Layer
countSeverityStepwise ss = sum $
                           map getSeverityByLayer $
                           filter hasCollision ss
  where
    deepest = deepestLayer ss

getSeverityByLayer :: Scanner -> Int
getSeverityByLayer (Scanner layer depth _ _) = layer * depth

--hasCollision _ _ = False
hasCollision :: Scanner -> Bool
hasCollision s = (getPos state) == 0
  where
    layer = getLayer s
    state = iterate step s !! layer

step :: Scanner -> Scanner
step s = (Scanner (getLayer s) d newPos newDir)
  where
    d = getDepth s
    newPos = nextPos (getPos s) (getDir s)
    newDir = nextDir newPos d (getDir s)

nextDir :: Position -> Depth -> Direction -> Direction
nextDir position d direction
  | position == 0 = Down
  | position == (d - 1) = Up
  | otherwise = direction

nextPos :: Position -> Direction -> Position
nextPos position Up = position - 1
nextPos position Down = position + 1

getDepth :: Scanner -> Depth
getDepth (Scanner _ depth _ _) = depth

getDir :: Scanner -> Direction
getDir (Scanner _ _ _ direction) = direction

getPos :: Scanner -> Position
getPos (Scanner _ _ position _) = position

getLayer :: Scanner -> Layer
getLayer (Scanner layer _ _ _) = layer

updatePos :: Direction -> Position -> Position
updatePos Down p = p + 1
updatePos Up p = p - 1

deepestLayer :: [Scanner] -> Layer
deepestLayer ss = maximum $ map getLayer ss

getScanner :: Layer -> [Scanner] -> Scanner
getScanner layer (s:ss)
  | layer == (getLayer s) = s
  | otherwise = getScanner layer ss
