module Day16 where
import Lib (apply, fileToLine)
import Data.String (lines)

data Instruction = Spin Int | Exchange Int Int | Partner Char Char deriving (Show)

day16_1 = apply (dance ['a'..'p']) fileContents
  where
    fileContents = fileToLine path
    path = "inputs/day16.txt"

dance :: String -> String -> String
dance programs contents = run instructions programs
  where
    instructions = (map parseInstruction) $ lines $ (map commaToNL) contents

run :: [Instruction] -> String -> String
run is ps = foldl (applyInstruction) ps is

parseInstruction :: String -> Instruction
parseInstruction ('s':spins) = Spin (read spins)
parseInstruction ('x':xs) = Exchange (read $ beforeSlash xs) (read $ afterSlash xs)
parseInstruction ('p':a:'/':b:[]) = Partner a b

beforeSlash ss = takeWhile (/= '/') ss
afterSlash ss = tail $ dropWhile (/= '/') ss

applyInstruction :: String -> Instruction -> String
applyInstruction s (Spin n) = let fromEnd = (length s) - n in (drop fromEnd s) ++ (take fromEnd s)
applyInstruction s (Partner a b) = swapElements a b s
applyInstruction s (Exchange x y) = swapIndices x y s

swapElements _ _ [] = []
swapElements x y (e:es)
  | e == x = y:(swapElements x y es)
  | e == y = x:(swapElements x y es)
  | otherwise = e:(swapElements x y es)

swapIndices x y es =
  let maxIndex = (length es) - 1
      indices = [0..maxIndex]
      adjusted = swapElements x y indices
      indexed = zip [0..maxIndex] es
  in
    map (\ix -> getValue ix indexed) adjusted

getValue :: Eq a => a -> [(a, b)] -> b
getValue targetKey ((k, v):xs)
  | targetKey == k = v
  | otherwise = getValue targetKey xs

commaToNL x
  | x == ',' = '\n'
  | otherwise = x
