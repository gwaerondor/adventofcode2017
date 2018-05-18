module Day18 where
import Data.String (words)

data Instruction = Set String Int
                 | Add String Int
                 | Multiply String String
                 | Modulo String Int
                 | Sound String
                 | Recover String
                 | JumpGZ String Int
                 deriving (Show)

data Register = Register String Int deriving (Show)

instance Eq Register where
  (==) (Register s1 _) (Register s2 _)
    | s1 == s1 = True
    | otherwise = False

firstRecoveredSound :: [String] -> [Instruction]
firstRecoveredSound is = executeUntilRecover instructions initialState
  where
    instructions = parseInstructions is
    initialState = initiateRegistry instructions

executeUntilRecover :: [Instruction] -> [Register] -> Int
executeUntilRecover is rs = execute is 0 rs

eur :: [Instruction] -> Int -> [Register] -> Int
eur is ix rs = case is of (Recover s) -> findRegister rs s
                          _ -> eur is (ix+1) rs
  where
    ci = is !! ix

parseInstructions :: [String] -> [Instruction]
parseInstructions is = map toInstruction $ map words is

toInstruction :: [String] -> Instruction
toInstruction ["set", r, x] = Set r (read x)
toInstruction ["add", r, x] = Add r (read x)
toInstruction ["mul", r1, r2] = Multiply r1 r2
toInstruction ["mod", r, x] = Modulo r (read x)
toInstruction ["snd", r] = Sound r
toInstruction ["rcv", r] = Recover r
toInstruction ["jgz", r, x] = JumpGZ r (read x)

modifiedRegisters :: Instruction -> [String]
modifiedRegisters (Set r _) = [r]
modifiedRegisters (Add r _) = [r]
modifiedRegisters (Multiply r1 r2) = [r1, r2]
modifiedRegisters (Modulo r _) = [r]
modifiedRegisters (Sound r) = [r]
modifiedRegisters (Recover r) = [r]
modifiedRegisters (JumpGZ r _) = [r]

initiateRegistry :: [Instruction] -> [Register]
initiateRegistry is = concat $ map createRegister is

createRegister :: Instruction -> [Register]
createRegister i = map (\x -> Register x 0) (modifiedRegisters i)

findRegister :: [Register] -> String -> Register
findRegister rs r = head $ filter (\(Register x _) -> x == r) rs

updateRegister :: [Register] -> Register -> [Register]
updateRegister (oldR:rs) newR
  | oldR == newR = newR:rs
  | otherwise = oldR:(updateRegister rs newR)
