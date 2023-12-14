module Day12 where

import qualified Data.HashMap.Lazy as M
import Data.List (intercalate, nub)
import qualified Data.Set as S

type Section = (String, [Int])

sample_ =
  [ "???.### 1,1,3",
    ".??..??...?##. 1,1,3",
    "?#?#?#?#?#?#?#? 1,3,1,6",
    "????.#...#... 4,1,1",
    "????.######..#####. 1,6,5",
    "?###???????? 3,2,1"
  ]

f = "???? 3"

sample = parseInput $ unlines sample_

parseLine :: String -> Section
parseLine line =
  let [springs, crc] = words line
   in (springs, map read . words . map (\a -> if a == ',' then ' ' else a) $ crc)

parseInput = map parseLine . lines

satisfies :: String -> String -> Bool
satisfies [] [] = True
satisfies ('?' : est) (a : ctual) = satisfies est ctual
satisfies ('.' : est) ('.' : ctual) = satisfies est ctual
satisfies ('#' : est) ('#' : ctual) = satisfies est ctual
satisfies test actual = False

generateMemo :: String -> [Int] -> [String]
generateMemo template' ns' = generate template' ns'
  where
    generate template [n] =
            filter (satisfies template)
              . map (\u -> replicate u '.' ++ replicate n '#' ++ replicate (length template - n - u) '.')
              $ [0 .. (length template - n)]
    generate template (first : rest) =
      let restMaxLength = length template - first - 1
          restMinLength = sum rest + length rest - 1
          firstMaxLength = length template - restMinLength - 1
          firstMinLength = length template - restMaxLength - 1
          firstOptions = concatMap (\l -> generateMemo (take l template) [first]) [firstMaxLength, firstMaxLength - 1 .. firstMinLength]
          restOptions = map (\fo -> (fo, generateMemo (drop (length fo + 1) template) rest)) firstOptions
          allOptions = concatMap (\(fo, ros) -> map (\ro -> fo ++ "." ++ ro) ros) restOptions
      in  nub $ filter (satisfies template) allOptions

getAnswerA = show . sum . map (\(template, ns) -> length $ generateMemo template ns) . parseInput

getAnswerB = show . sum . map (\(template, ns) -> length $ generateMemo (intercalate "?" $ replicate 5 template) (concat $ replicate 5 ns)) . parseInput