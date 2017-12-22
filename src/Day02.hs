module Day02 where
import Lib (fileToLines, apply)
import Data.List.Split (splitOn)

day02_1 :: IO String
day02_1 = apply checksum fileContents
  where
    fileContents = fileToLines path
    path = "/mnt/c/Users/Robin/adventofcode2017/inputs/day02.txt"

checksum :: [String] -> Int
checksum css = (calculate . transform) css

transform :: [String] -> [[Int]]
transform xs = map (map read) $ map (splitOn "\t") xs

calculate :: [[Int]] -> Int
calculate [] = 0
calculate (xs:xss) = diff + (calculate xss)
  where
    diff = (maximum xs) - (minimum xs)
