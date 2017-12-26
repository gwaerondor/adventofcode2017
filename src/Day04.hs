module Day04 where
import Lib (fileToLines, apply)
import Data.List (nub, sort)

day04_1 :: IO String
day04_1 = run hasNoDuplicates

day04_2 :: IO String
day04_2 = run hasNoAnagrams

run condition = apply (countValidPassphrases condition) fileContents
  where
    fileContents = fileToLines path
    path = "/mnt/c/Users/Robin/adventofcode2017/inputs/day04.txt"

countValidPassphrases :: (String -> Bool) -> [String] -> Int
countValidPassphrases condition pws = length $ filter condition pws

hasNoDuplicates :: String -> Bool
hasNoDuplicates pp = (words pp) == (nub (words pp))

hasNoAnagrams :: String -> Bool
hasNoAnagrams pp = sorted == (nub sorted)
  where
    sorted = map sort $ words pp
