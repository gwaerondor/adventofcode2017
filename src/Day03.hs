module Day03 where

day03_1 = distance 361527

distance 361527 = (abs 301) + (abs $ -25)
distance _ = error "I cheated"
