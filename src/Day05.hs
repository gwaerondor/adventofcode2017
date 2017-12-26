module Day05 where
import Lib (fileToLines, apply)
import Data.List ((!!))

day05_1 :: IO String
day05_1 = apply (getTerminationIndex . transform) contents
  where
    contents = fileToLines path
    path = "/mnt/c/Users/Robin/adventofcode2017/inputs/day05.txt"

transform :: [String] -> [Int]
transform cs = map read cs

getTerminationIndex :: [Int] -> Int
getTerminationIndex xs = gti xs 1 0

gti :: [Int] -> Int -> Int -> Int
gti xs i steps
  | i > length xs = steps
  | otherwise = gti (update xs i) nextIndex (steps + 1)
  where
    nextIndex = i + xs !! (i-1)

update :: [Int] -> Int -> [Int]
update xs i = incrementAt i xs

incrementAt 1 (x:xs) = (x+1):xs
incrementAt i (x:xs) = x:(incrementAt (i-1) xs)
