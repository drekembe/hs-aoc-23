module Day9 where

getAnswerA = show . sum . map getNextNumber' . parseInput

getAnswerB = show . sum . map (getNextNumber' . reverse) . parseInput

parseInput = map (map read . words) . lines

getNextNumber ls =
  let nextSequence = zipWith (-) (tail ls) ls
   in if all (== 0) ls
        then 0
        else last ls + getNextNumber nextSequence

getNextNumber' = foldr ((+) . last) 0
        . takeWhile (any (/= 0))
        . iterate (\x -> zipWith (-) (tail x) x)