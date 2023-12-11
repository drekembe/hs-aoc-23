module Day8 (getAnswer, instructions) where

import Data.List
import qualified Data.Map as M

type Direction = (Node, Node) -> Node

type Node = String

type Map = M.Map Node (Node, Node)

type Instruction = (Node, (Node, Node))

data Input = Input
  { directions :: [Direction],
    instructions :: [Instruction]
  }


parseInput :: [String] -> Input
parseInput input =
  let directionsString = head input
      instructionsList = drop 2 input
   in Input
        { directions = parseDirections directionsString,
          instructions = parseInstructions instructionsList
        }

parseDirections :: String -> [Direction]
parseDirections = map (\x -> if x == 'L' then fst else snd)

parseInstructions :: [String] -> [Instruction]
parseInstructions = map parseInstruction 

parseInstruction :: String -> Instruction
parseInstruction (a:b:c:' ':'=':' ':'(':d:e:f:',':' ':g:h:i:')':_) = ([a,b,c], ([d,e,f],[g,h,i]))
parseInstruction _ = undefined

getAnswer :: [String] -> Input
getAnswer = parseInput 