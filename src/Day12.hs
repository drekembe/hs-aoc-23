module Day12 where

import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (intercalate)

type Section = ([Char], [Int])

type Cache = M.Map Section Int

parseLine :: String -> Section
parseLine st = (springs, map read $ splitOn "," groups)
  where
    [springs, groups] = splitOn " " st


evalSection :: Section -> Int
evalSection (springs, groups) = fst $ go M.empty springs groups
  where
    go :: Cache -> [Char] -> [Int] -> (Int, Cache)
    go cache [] [] = (1, cache)
    go cache [] [n] = (0, cache)
    go cache springs [] = (if '#' `elem` springs then 0 else 1, cache)
    go cache ('.' : rest) groups = go cache rest groups
    go cache ('?' : rest) groups = (res + res', cache' `M.union` cache'')
      where (res, cache') = case M.lookup (rest, groups) cache of
              Nothing -> go cache rest groups
              (Just foundRes) -> (foundRes, cache)
            (res', cache'') = case M.lookup ('#':rest, groups) cache' of
              Nothing -> go cache' ('#':rest) groups
              (Just foundRes') -> (foundRes', cache')
    go cache springs (g : roups)
      | length springs >= g && 
        notElem '.' (take g springs) && 
        notElem '#' (take 1 (drop g springs)) = (res, M.insert (springs', roups) res cache')
      where 
        springs' = drop (g + 1) springs
        (res, cache') = case M.lookup (springs', roups) cache of
          Nothing -> go cache (drop (g + 1) springs) roups
          (Just foundRes) -> (foundRes, cache)
    go cache _ _ = (0, cache)

expand :: Section -> Section
expand (springs, groups) = (intercalate "?" (replicate 5 springs), concat $ replicate 5 groups)

parseInput = map parseLine . lines

getAnswerA = show . sum . map evalSection . parseInput

getAnswerB = show . sum . map (evalSection . expand) . parseInput