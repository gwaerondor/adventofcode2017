module Day17 where

day17_1 :: [Int]
day17_1 = spinlock 314

spinlock :: Int -> [Int]
spinlock steps = take 2 $ last $ take 2018 $ iterate (run steps) [0]

run steps xs = (newElem:newTail) ++ newHead
  where
    newPos = case (length xs) of
               0 -> 0
               _ -> 1 + steps `mod` (length xs)
    newHead = take newPos xs
    newTail = drop newPos xs
    newElem = (length xs)
