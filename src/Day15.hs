{-# LANGUAGE BangPatterns #-}

module Day15 where
import Data.Bits ((.&.))

day15_1 :: Int
day15_1 = judge initA initB 0 40000000
  where
    initA = 618
    initB = 814      

sixteenBits :: Int
sixteenBits = 65535

bitsMatch :: Int -> Int -> Bool
bitsMatch a b = (a .&. sixteenBits) == (b .&. sixteenBits)

nextValueA :: Int -> Int
nextValueA = (nextValue 16807)

nextValueB :: Int -> Int
nextValueB = (nextValue 48271)

nextValue :: Int -> Int -> Int
nextValue factor x = (factor * x) `mod` 2147483647

judge :: Int -> Int -> Int -> Int -> Int
judge _ _ acc 0 = acc
judge !a !b !acc !iterations = (judge nextA nextB (acc + (judge' nextA nextB)) (iterations - 1))
  where
    nextA = nextValueA a
    nextB = nextValueB b

judge' a b
  | bitsMatch a b = 1
  | otherwise = 0

--------------------------------------------------------------------------------
nextValueA2 :: Int -> Int
nextValueA2 x
  | nv `mod` 4 == 0 = nv
  | otherwise = nextValueA2 nv
  where
    nv = nextValueA x

nextValueB2 :: Int -> Int
nextValueB2 x
  | nv `mod` 8 == 0 = nv
  | otherwise = nextValueB2 nv
  where
    nv = nextValueB x

judge2 :: Int -> Int -> Int -> Int -> Int
judge2 _ _ acc 0 = acc
judge2 !a !b !acc !iterations = (judge2 nextA nextB (acc + (judge' nextA nextB)) (iterations - 1))
  where
    nextA = nextValueA2 a
    nextB = nextValueB2 b

day15_2 :: Int
day15_2 = judge2 initA initB 0 5000000
  where
    initA = 618
    initB = 814      
