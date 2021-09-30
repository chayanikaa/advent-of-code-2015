module Seven
    (
    part1,
    readInstruction,
    readInstructions,
    processInstruction,
    Instruction (..),
    ) where

import Data.List.Split
import Data.List
import Data.Bits
import Text.Regex.Posix
import Data.Maybe
import Data.Word

data Wire = Wire String Word16 deriving (Show)
getName :: Wire -> String
getName (Wire name _) = name
getValue :: Wire -> Word16
getValue (Wire _ value) = value

data Instruction = Instruction String String deriving (Show)
getInput :: Instruction -> String
getInput (Instruction input _) = input
getOutput :: Instruction -> String
getOutput (Instruction _ output) = output

data Expression = Expression String String String deriving (Show)
getOperator :: Expression -> String
getOperator (Expression op _ _) = op
getOperand1 :: Expression -> String
getOperand1 (Expression _ op1 _) = op1
getOperand2 :: Expression -> String
getOperand2 (Expression _ _ op2) = op2

part1 :: [String] -> String -> String
part1 instructionStrings wireName =
  let instructions = readInstructions instructionStrings
      wires = processInstructions instructions []
  in subValue wires wireName
-- applyInstruction :: String [Wire] -> 

readInstructions :: [String] -> [Instruction]
readInstructions = map readInstruction 

readInstruction :: String -> Instruction
readInstruction str =
  let parts = splitOn " -> " str
  in Instruction (parts!!0) (parts!!1)

getWires :: [Instruction] -> [Wire]
getWires instructions =
  map (\i -> Wire (getOutput i) (read (getInput i) :: Word16)) instructions

-- run recursively till all instructions processed
processInstructions :: [Instruction] -> [Wire] -> [Wire]
processInstructions instructions wires =
  let (solved, unsolved) = partition (\i -> ((getInput i) =~ "^[0-9]+$" :: Bool)) instructions
      newWires = wires ++ (getWires solved)
  in if (null unsolved)
     then newWires
     else processInstructions (map (processInstruction newWires) unsolved) newWires

-- parses the input into an expression
processInstruction :: [Wire] -> Instruction -> Instruction
processInstruction wires instruction =
  let expression = parseInstruction $ getInput instruction
      processedExpression = Expression (getOperator expression) (subValue wires (getOperand1 expression)) (subValue wires (getOperand2 expression))
      solvedInput = solveExpression processedExpression
  in Instruction solvedInput (getOutput instruction)
  
parseInstruction :: String -> Expression
parseInstruction str
  | str =~ "^NOT [a-z0-9]+$" :: Bool = Expression "NOT" ((words str)!!1) ""
  | str =~ "^[a-z0-9]+ [A-Z]+ [a-z0-9]+$" :: Bool = Expression ((words str)!!1) ((words str)!!0) ((words str)!!2)
  | str =~ "^[a-z]+$" :: Bool = Expression "ID" str ""
  | otherwise = error $ "Invalid input to create an expression " ++ str

solveExpression :: Expression -> String
solveExpression expression
  | ((getOperand1 expression) =~ "^[0-9]+$" :: Bool) && ((getOperand2 expression) =~ "^[0-9]+$" :: Bool) = calculate expression
  | (getOperator expression) == "NOT" && ((getOperand1 expression) =~ "^[0-9]+$" :: Bool) = calculate expression
  | (getOperator expression) == "NOT" = "NOT " ++ (getOperand1 expression)
  | (getOperator expression) == "ID" = getOperand1 expression
  | otherwise = (getOperand1 expression) ++ " " ++ (getOperator expression) ++ " " ++ (getOperand2 expression)

subValue :: [Wire] -> String -> String
subValue wires wireName =
  let maybeWire = find (\wire -> (getName wire) == wireName) wires
  in if (isNothing maybeWire) then wireName
        else show $ getValue (fromJust maybeWire)

calculate :: Expression -> String
calculate expression
  | (getOperator expression) == "NOT" = show $ complement (read (getOperand1 expression) :: Word16)
  | (getOperator expression) == "AND" = show $ (.&.) (read (getOperand1 expression) :: Word16) (read (getOperand2 expression) :: Word16)
  | (getOperator expression) == "LSHIFT" = show $ shiftL (read (getOperand1 expression) :: Word16) (read (getOperand2 expression) :: Int)
  | (getOperator expression) == "RSHIFT" = show $ shiftR (read (getOperand1 expression) :: Word16) (read (getOperand2 expression) :: Int)
  | (getOperator expression) == "OR" = show $ (.|.) (read (getOperand1 expression) :: Word16) (read (getOperand2 expression) :: Word16)
  | otherwise = error $ "Invalid operator " ++ (show expression)
