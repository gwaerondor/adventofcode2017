module Day06 where
import Data.List (elemIndices, nub, (!!))

day06_1 :: Int
day06_1 = cyclesBeforeLoop input
  where
    input = [11, 11, 13, 7, 0, 15, 5, 5, 4, 4, 1, 1, 7, 1, 15, 11]

-- I can optimise this by only counting the amount of iterations before the initial state is obtained again
-- Don't need to keep all the old states in this example.
day06_2 :: Int
day06_2 = cyclesBeforeLoop input
  where
    input = (iterate redistribute [11, 11, 13, 7, 0, 15, 5, 5, 4, 4, 1, 1, 7, 1, 15, 11]) !! firstRepeatedIndex
    firstRepeatedIndex = 4074

cyclesBeforeLoop :: [Int] -> Int
cyclesBeforeLoop input = cbl input [] 0

cbl :: [Int] -> [[Int]] -> Int -> Int
cbl xs states cycles
  | nextState == (nub nextState) = cbl (redistribute xs) nextState (cycles + 1)
  | otherwise = cycles
  where
    nextState = (xs:states)

redistribute :: [Int] -> [Int]
redistribute xs = putFromIndex ix amount $ emptyLargest xs
  where
    ix = 1 + (indexOfMax xs)
    amount = maximum xs

putFromIndex _ 0 xs = xs
putFromIndex ix n xs = putFromIndex (ci + 1) (n - 1) $ incrementIndex ci xs
  where
    ci = case ix >= (length xs) of
           True -> 0
           False -> ix
    

indexOfMax :: [Int] -> Int
indexOfMax xs = head $ elemIndices (maximum xs) xs

emptyLargest :: [Int] -> [Int]
emptyLargest xs = updateIndex (indexOfMax xs) xs (\_ -> 0)

incrementIndex :: Int -> [Int] -> [Int]
incrementIndex i xs = updateIndex i xs (+1)

updateIndex :: Int -> [Int] -> (Int -> Int) -> [Int]
updateIndex 0 (x:xs) f = (f x):xs
updateIndex n (x:xs) f = x:(updateIndex (n - 1) xs f)
