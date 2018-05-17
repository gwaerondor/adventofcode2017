{-# LANGUAGE BangPatterns #-}
module Day10 where
import Data.Char (ord)
import Data.Bits (xor)
import Text.Printf (printf)

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
hash' !xs [] _ !shifts 1 _ = unshift shifts xs
hash' !xs [] !skips !shifts !rounds !initialInput = hash' xs initialInput skips shifts (rounds - 1) initialInput
hash' !xs (!il:(!ils)) !skips !shifts !rounds !initialInput = hash' newXs ils newSkip newShifts rounds initialInput
  where
    !tied = reverseFirst il xs
    !newXs = shift (il + skips) (tied `seq` tied)
    !newSkip = skips + 1
    !newShifts = shifts + il + skips

tie :: [a] -> Length -> [a]
tie !xs !length = reverseFirst length xs

reverseFirst :: Length -> [a] -> [a]
reverseFirst !n !xs = (reverse $ take n xs) ++ (drop n xs)

shift :: Int -> [a] -> [a]
shift !steps !xs =
  let s = (drop n xs) ++ (take n xs) in (s `seq` s)
  where
    !n = steps `mod` (length xs)

unshift :: Int -> [a] -> [a]
unshift 0 !xs = xs
unshift !steps !xs = unshift (steps - 1) ((last xs):(init xs))

--------------------------------------------------------------------------------
-- I give up. Cannot run day10_2 due to failing to allocate enough stack memory
-- in Windows. Bangpatterns and `seq` didn't seem to help; even though this
-- function is as far as I can tell tail recursive, something is building up.
-- I'll see later if I can run it without any problems in Linux,
-- but it'll probably end up with a similar problem.
day10_2 :: String
day10_2 = toHex $ hash2 [0..255] ils
  where
    ils = (map ord "70,66,255,2,48,0,54,48,80,141,244,254,160,108,1,41")

hash2 !inputList !inputLengths = denseHash $ hash' inputList adjustedILs skips shifts rounds adjustedILs
  where
    adjustedILs = inputLengths ++ [17, 31, 73, 47, 23]
    skips = 0
    shifts = 0
    rounds = 64

denseHash :: [Int] -> [Int]
denseHash xs = map (foldl xor 0) $ chunk 16 xs

toHex :: [Int] -> String
toHex x = concat $ map (printf "%02x") x

chunk :: Int -> [a] -> [[a]]
chunk n xs
  | length xs > n = (take n xs) : (chunk n (drop n xs))
  | otherwise = [xs]
