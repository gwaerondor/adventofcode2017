module Day01 where

import Lib (fileToLine, apply)
import Data.Char (digitToInt)

day01_1 :: IO String
day01_1 = run checksum

day01_2 :: IO String
day01_2 = run checksum2

run f = apply f fileContents
  where
    fileContents = fileToLine path
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

---

checksum2 :: String -> Int
checksum2 cs = 2 * (checksum2' . splitInTwo) numbers
  where
    numbers = map digitToInt cs

splitInTwo :: [a] -> ([a], [a])
splitInTwo xs = ((take half xs), (drop half xs))
  where
    half = (length xs) `div` 2

checksum2' :: ([Int], [Int]) -> Int
checksum2' ([], []) = 0
checksum2' ((x:xs), (y:ys))
  | x == y = (checksum2' (xs, ys)) + x
  | otherwise = checksum2' (xs, ys)
