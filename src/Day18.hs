module Day18 where

import Data.List.Split (splitOn)

data Dir = R | D | L | U deriving (Show, Eq, Ord)
type Instructions = [(Dir, Int, String)]

parseInput :: String -> Instructions
parseInput = map pl . lines 
  where pl l = let [dir, n, color] = splitOn " " l in (pd dir, read n, takeWhile (/=')') $ drop 2 color)
        pd "R" = R
        pd "L" = L
        pd "U" = U
        pd "D" = D
        pd _  = error "whoops"

move R (x,y) = (x+1, y)
move L (x,y) = (x-1, y)
move D (x,y) = (x, y+1)
move U (x,y) = (x, y-1)

outline :: Instructions -> [(Int, Int)]
outline xs = go xs (0,0)
  where
    go [] (x,y) = []
    go ((dir, steps, color):rest) (x,y) = let i = take steps (iterate (move dir) (x,y)) in i ++ go rest (move dir (last i))

getAnswerA = show . outline . parseInput
getAnswerB _ = ""