module Day14 where

import Day10 (hash2, toHex)
import Data.Char (ord)

day14_1 :: String
day14_1 = toHex $ hash2 [0..255] (head ils)
  where
    inputWord = "amgozmfv-"
    inputs = map (\d -> inputWord ++ (show d)) [0..127]
    ils = map (map ord) inputs
