module Day06 where

day06_1 :: Int
day06_1 = cyclesBeforeLoop input
  where
    input = [11, 11, 13, 7, 0, 15, 5, 5, 4, 4, 1, 1, 7, 1, 15, 11]

cyclesBeforeLoop :: [Int] -> Int
cyclesBeforeLoop _ = -1
