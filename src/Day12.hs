module Day12 where

import Data.List
import qualified Data.Set as S

type Section = (String, [Int])

sample_ = [
  "???.### 1,1,3",
  ".??..??...?##. 1,1,3",
  "?#?#?#?#?#?#?#? 1,3,1,6",
  "????.#...#... 4,1,1",
  "????.######..#####. 1,6,5",
  "?###???????? 3,2,1"]

f = "???? 3"


sample = parseInput $ unlines sample_

parseLine :: String -> Section
parseLine line = 
  let [springs, crc] = words line
  in  (springs, map read . words . map (\a -> if a == ',' then ' ' else a) $ crc )
parseInput = map parseLine . lines

satisfies :: String -> String -> Bool
satisfies [] [] = True
satisfies ('?':est) (a:ctual) = satisfies est ctual
satisfies ('.':est) ('.':ctual) = satisfies est ctual
satisfies ('#':est) ('#':ctual) = satisfies est ctual
satisfies test actual = False

generate :: String -> [Int] -> [String]
generate template [n] 
  | length template == n = [replicate n '#']
  | otherwise = filter (satisfies template ) 
    . map (\u -> replicate u '.' ++ replicate n '#' ++ replicate (length template - n - u) '.') 
    $ [0..(length template - n)]
generate template (first:rest) =
  let restMaxLength = length template - first - 1
      restMinLength = sum rest + length rest - 1
      firstMaxLength = length template - restMinLength - 1
      firstMinLength = length template - restMaxLength - 1
      firstOptions = concatMap (\l -> generate (take l template) [first]) [firstMinLength .. firstMaxLength]
      restOptions = map (\fo -> (fo, generate (drop (length fo + 1) template) rest)) firstOptions
      allOptions = concatMap (\(fo, ros) -> map (\ro -> fo ++ "." ++ ro) ros) restOptions
  in  filter (satisfies template) allOptions

getAnswerA = sum . map (\(template, ns) -> length $ generate template ns) . parseInput
getAnswerB = sum . map (\(template, ns) -> length $ generate (concat $ replicate 5 template) (concat $ replicate 5 ns)) . parseInput