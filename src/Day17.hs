module Day17 where

day17_1 :: [Int]
day17_1 = take 2 $ spinlock 314 0 2017 [0]

spinlock :: Int -> Int -> Int -> [Int] -> [Int]
spinlock steps currentIter targetIter res
  | currentIter == targetIter = res
  | otherwise = res `seq`
                currentIter `seq`
                spinlock steps (currentIter + 1) targetIter (run steps res)

run :: Int -> [Int] -> [Int]
run steps xs = xs `seq` (newElem:newTail) ++ newHead
  where
    newPos = 1 + steps `mod` (length xs)
    newHead = take newPos xs
    newTail = drop newPos xs
    newElem = (length xs)

day17_2 :: Int
day17_2 = head $ afterZero $ spinlock 314 0 fiftyMillion [0]

fiftyMillion :: Int
fiftyMillion = 50000000

afterZero :: [Int] -> [Int]
afterZero (0:xs) = xs
afterZero (_:xs) = afterZero xs
