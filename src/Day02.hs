module Day02 where
import Lib (fileToLines, apply)
import Data.List ((!!), (\\))
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

---

checksum2 :: [String] -> Int
checksum2 css = (calculate2 . transform) css

calculate2 :: [[Int]] -> Int
calculate2 xss = sum $ map quota $ map findDivisible xss

quota :: (Int, Int) -> Int
quota (x, y) = x `div` y

findDivisible :: [Int] -> (Int, Int)
findDivisible xs = findDivisibleByIndex 0 xs

findDivisibleByIndex :: Int -> [Int] -> (Int, Int)
findDivisibleByIndex i xs
  | filtered == [] = findDivisibleByIndex (i + 1) xs
  | otherwise = (head $ filtered, x)
  where
    x = xs !! i
    filtered = filter (\n -> (n `mod` x) == 0) (filter (/= x) xs)
