module Day10 where
import Data.Char (ord)

type Step = Int
type Skip = Int
type Length = Int

day10_1 :: Int
day10_1 = product $ take 2 $ hash [0..255] ils
  where
    ils = [70,66,255,2,48,0,54,48,80,141,244,254,160,108,1,41]

--------------------------------------------------------------------------------
hash inputList inputLengths = hash' inputList inputLengths skips shifts rounds inputLengths
  where
    skips = 0
    shifts = 0
    rounds = 1

hash' :: [Int] -> [Length] -> Skip -> Int -> Int -> [Length] -> [Int]
hash' xs [] _ shifts 1 _ = unshift shifts xs
hash' xs [] skips shifts rounds initialInput = hash' xs initialInput skips shifts (rounds - 1) initialInput
hash' xs (il:ils) skips shifts rounds initialInput = hash' newXs ils newSkip newShifts rounds initialInput
  where
    tied = reverseFirst il xs
    newXs = shift (il + skips) tied
    newSkip = skips + 1
    newShifts = shifts + il + skips

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

--------------------------------------------------------------------------------
day10_2 :: String
day10_2 = toHex $ hash2 [0..255] ils
  where
    ils = (map ord "70,66,255,2,48,0,54,48,80,141,244,254,160,108,1,41") ++ [17, 31, 73, 47, 23]

hash2 inputList inputLengths = denseHash $ hash' inputList inputLengths skips shifts rounds inputLengths
  where
    skips = 0
    shifts = 0
    rounds = 64

denseHash :: [Int] -> [Int]
denseHash xs = error "not finished!"

toHex :: [Int] -> String
toHex _ = error "not finished!"
