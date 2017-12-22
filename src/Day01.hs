module Day01 where

import Lib (fileToLines, apply)
import Data.Char (digitToInt)

day01_1 :: IO String
day01_1 = apply checksum fileContents
  where
    fileContents = fmap head $ fileToLines path
    path = "/mnt/c/Users/Robin/adventofcode2017/inputs/day01_1.txt"

checksum :: String -> Int
checksum cs = (firstAndLast numbers) + (checksum' numbers)
  where
    numbers = map digitToInt cs

checksum' :: [Int] -> Int
checksum' (x:y:xs)
 | x == y = x + (checksum' (y:xs))
 | otherwise = checksum' (y:xs)
checksum' _ = 0

firstAndLast :: [Int] -> Int
firstAndLast xs
 | (head xs) == (last xs) = head xs
 | otherwise = 0
