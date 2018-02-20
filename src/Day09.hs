module Day09 where
import Lib (fileToLine, apply)

day09_1 :: IO String
day09_1 = apply score contents
  where
    contents = fileToLine path
    path = "inputs/day09.txt"

score :: String -> Int
score cs = parseScore 1 $ (removeCommas . removeGarbage . removeInvalidated) cs

removeInvalidated :: String -> String
removeInvalidated (c:(d:cs))
  | c == '!' = removeInvalidated cs
  | otherwise = c:(removeInvalidated (d:cs))
removeInvalidated cs = cs

removeGarbage :: String -> String
removeGarbage [] = []
removeGarbage (c:cs)
  | c == '<' = removeGarbage $ tail $ dropWhile (/= '>') cs
  | otherwise = c:(removeGarbage cs)

removeCommas :: String -> String
removeCommas = filter (/= ',')

parseScore :: Int -> String -> Int
parseScore _ [] = 0
parseScore level ('{':r) = level + (parseScore (level + 1) r)
parseScore level ('}':r) = parseScore (level - 1) r
