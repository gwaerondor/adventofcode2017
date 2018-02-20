module Day10 where

type Step = Int
type Skip = Int
type Length = Int

day10_1 :: ([Int], Int)
day10_1 = product $ take 2 $ hash [0..255] ils
  where
    ils = [70, 66, 255, 2, 48, 0, 54, 48, 80, 141, 244, 254, 160, 108, 1, 41]

hash inputList inputLengths = hash' inputList inputLengths skips shifts
  where
    skips = 0
    shifts = 0

hash' :: [Int] -> [Length] -> Skip -> Int -> [Int]
hash' xs [] _ shifts = unshift shifts xs
hash' xs (il:ils) s shifts = hash' newXs ils newSkip newShifts
  where
    tied = reverseFirst il xs
    newXs = shift (il + s) tied
    newSkip = s + 1
    newShifts = shifts + il + s

tie :: [a] -> Length -> [a]
tie xs length = reverseFirst length xs

reverseFirst :: Length -> [a] -> [a]
reverseFirst n xs = (reverse $ take n xs) ++ (drop n xs)

shift :: Int -> [a] -> [a]
shift steps xs = (drop n xs) ++ (take n xs)
  where
    n = steps `mod` (length xs) 

unshift :: Int -> [a] -> [a]
unshift 0 xs = xs
unshift steps xs = unshift (steps - 1) ((last xs):(init xs))
