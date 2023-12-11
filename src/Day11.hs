module Day11 where

import qualified Data.Map as M
import qualified Data.Set as S

type StarField = S.Set (Int, Int)

sampleInput :: [String]
sampleInput = ["...#......",
  ".......#..",
  "#.........",
  "..........",
  "......#...",
  ".#........",
  ".........#",
  "..........",
  ".......#..",
   "#...#....." ]

sampleInputS :: String
sampleInputS = unlines sampleInput

parseInput :: String -> StarField
parseInput input = 
  let ls = lines input
      lineLength = length $ head ls
  in  S.fromList $ concatMap (\y -> map (\x -> (x,y)) [0..(lineLength - 1)]) [0..(length ls - 1)]

getAnswerA = parseInput
getAnswerB = parseInput