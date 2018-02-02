module Day08 where
import Lib (fileToLines, apply)
import Data.List (sortBy, (!!), nub)
data Register = Register Name Value deriving (Eq, Show)
type Name = String
type Value = Int
instance Ord Register where
  compare (Register _ v1) (Register _ v2) = compare v1 v2

instance Show Instruction where
  show (Instruction t inc c comp) = unwords $ map show [t, show inc, c]

data Instruction = Instruction Name Increment Name Comparison
type Increment = Int
type Comparison = (Int -> Bool)

day08_1 :: IO String
day08_1 = apply (show . largestRegister . fst . run) contents
  where
    contents = fileToLines path
    path = "inputs/day08.txt"

day08_2 :: IO String
day08_2 = apply (show . snd . run) contents
  where
    contents = fileToLines path
    path = "inputs/day08.txt"

largestRegister :: [Register] -> Value
largestRegister rs = getValue $ head $ (sortBy (flip compare) rs)

getValue :: Register -> Value
getValue (Register _ v) = v

run :: [String] -> ([Register], Value)
run lines = execute instructions $ createAllRegisters instructions
  where
    instructions = map (parseInstruction . words) lines
    
createAllRegisters :: [Instruction] -> [Register]
createAllRegisters [] = []
createAllRegisters ((Instruction t _ c _):is) = nub $ concat $ [[Register t 0, Register c 0], createAllRegisters is]

parseInstruction :: [String] -> Instruction
parseInstruction ws = Instruction (ws !! 0) increment (ws !! 4) condition
  where
    increment = (parseIncrement ws)
    condition = (parseCondition ws)

parseIncrement :: [String] -> Int
parseIncrement ws = case (ws !! 1) of
                      "inc" ->
                        value
                      "dec" ->
                        - value
  where
    value = read (ws !! 2)

parseCondition :: [String] -> (Int -> Bool)
parseCondition ws = (flip $ toFunction (ws !! 5)) value
  where
    value = read (ws !! 6)

toFunction :: String -> (Int -> Int -> Bool)
toFunction "<" = (<)
toFunction "<=" = (<=)
toFunction ">" = (>)
toFunction ">=" = (>=)
toFunction "==" = (==)
toFunction "!=" = (/=)

execute :: [Instruction] -> [Register] -> ([Register], Value)
execute is regs = execute' is regs 0

execute' :: [Instruction] -> [Register] -> Value -> ([Register], Value)
execute' [] regs highest = (regs, highest)
execute' (i:is) regs highest = case checkCondition i regs of
                             True ->
                               execute' is (updateRegister i regs) newHighest
                             False ->
                               execute' is regs highest
  where
    updatedRegister = updateRegister i regs
    newRegisterValue (Instruction n _ _ _) = getRegisterValue n updatedRegister
    newHighest = max (newRegisterValue i) highest

checkCondition :: Instruction -> [Register] -> Bool
checkCondition (Instruction _ _ name cond) regs = cond (getRegisterValue name regs)

getRegisterValue :: String -> [Register] -> Value
getRegisterValue n regs = getValue $ findRegister n regs

findRegister :: Name -> [Register] -> Register
findRegister target [] = error ("Error: could not find register " ++ target)
findRegister target ((Register n v):rs)
  | target == n = Register n v
  | otherwise = findRegister target rs

updateRegister :: Instruction -> [Register] -> [Register]
updateRegister (Instruction n inc _ _) regs = newReg:(filter (\x -> getName x /= n) regs)
  where
    newValue = (getRegisterValue n regs) + inc
    newReg = Register n newValue

getName :: Register -> Name
getName (Register n _) = n
