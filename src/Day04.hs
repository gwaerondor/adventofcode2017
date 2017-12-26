module Day04 where
import Lib (fileToLines, apply)
import Data.List (nub)

day04_1 :: IO String
day04_1 = apply countValidPassphrases fileContents
  where
    fileContents = fileToLines path
    path = "/mnt/c/Users/Robin/adventofcode2017/inputs/day04.txt"

countValidPassphrases :: [String] -> Int
countValidPassphrases pws = length $ filter isValidPassphrase pws

isValidPassphrase :: String -> Bool
isValidPassphrase pp = (words pp) == (nub (words pp))
