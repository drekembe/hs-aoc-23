module Day12 where

import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S

type Section = ([Char], [Int])

sample_ =
  [ "???.### 1,1,3",
    ".??..??...?##. 1,1,3",
    "?#?#?#?#?#?#?#? 1,3,1,6",
    "????.#...#... 4,1,1",
    "????.######..#####. 1,6,5",
    "?###???????? 3,2,1"
  ]

parseLine :: String -> Section
parseLine st = (springs, map read $ splitOn "," groups)
  where
    [springs, groups] = splitOn " " st

evalSection :: Section -> Int
evalSection (springs, groups) = go springs groups
  where
    go :: [Char] -> [Int] -> Int
    go [] [] = 1
    go [] [n] = 0
    go springs [] = if '#' `elem` springs then 0 else 1
    go ('.' : rest) groups = go rest groups
    go ('?' : rest) groups = go rest groups + go ('#' : rest) groups
    go springs (g : roups) 
      | length springs >= g && notElem '.' (take g springs) && notElem '#' (take 1 (drop g springs)) = go (drop (g + 1) springs) roups
    go _ _ = 0

parseInput = map parseLine . lines

getAnswerA = show . sum . map evalSection . parseInput

getAnswerB _ = "ok"