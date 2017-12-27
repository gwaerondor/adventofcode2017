module Day05 where
import Lib (fileToLines, apply)
import Data.Array (Array, listArray, (//), (!), bounds)

day05_1 :: IO String
day05_1 = apply (getTerminationIndex . transform) contents
  where
    contents = fileToLines path
    path = "/mnt/c/Users/Robin/adventofcode2017/inputs/day05.txt"

transform :: [String] -> [Int]
transform cs = map read cs

getTerminationIndex :: [Int] -> Int
getTerminationIndex xs = gti (listArray (1, length xs) xs) 1 0

gti :: Array Int Int -> Int -> Int -> Int
gti xs i steps
  | i > size = steps
  | otherwise = ((gti $! incremented) $! nextIndex) $! nextStep
  where
    incremented = incrementAt i xs
    nextIndex = i + xs ! i
    nextStep = 1 + steps
    size = snd $ bounds xs

incrementAt :: Int -> Array Int Int -> Array Int Int
incrementAt i xs = case x >= 3 of
                     True -> xs // [(i, x - 1)]
                     False -> xs // [(i, x + 1)]
-- incrementAt i xs = xs // [(i, x+1)]
  where
    x = xs ! i
