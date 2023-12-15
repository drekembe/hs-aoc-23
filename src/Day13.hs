module Day13 where

import qualified Data.Array as A
import Data.List.Split (splitOn)
import Data.Maybe (isJust, fromJust)
import Data.Foldable (toList)
import Util (getColumn, getRow)
import Data.List

type Mirror = A.Array (Int, Int) Char

parseInput :: String -> [Mirror]
parseInput = map parseMirror . splitOn [""] . lines

parseMirror :: [String] -> Mirror
parseMirror lines = A.array ((0,0), (length (head lines) - 1 , length lines - 1))
  . concat
  . zipWith (\y line -> zipWith (\x ch -> ((x,y), ch)) [0..] line) [0..]
  $ lines

getColumnSolution mirror delimiter = sum [delimiter + 1 | all (\(c1,c2) -> getColumn c1 mirror == getColumn c2 mirror ) compares]
  where (_, (end, _)) = A.bounds mirror
        toEnd = end - delimiter
        fromStart = delimiter + 1
        reflectionWidth = min toEnd fromStart
        compares = zip (reverse [delimiter-reflectionWidth+1..delimiter]) [delimiter+1..delimiter+reflectionWidth]

getRowSolution mirror delimiter = sum [delimiter + 1 | all (\(r1,r2) -> getRow r1 mirror == getRow r2 mirror) compares]
  where (_, (_, end)) = A.bounds mirror
        toEnd = end - delimiter
        fromStart = delimiter + 1
        reflectionWidth = min toEnd fromStart
        compares = zip (reverse [delimiter-reflectionWidth+1..delimiter]) [delimiter+1..delimiter+reflectionWidth]

getColumnSolutions mirror = sum columnSolutions
  where (_, (xEnd, yEnd)) = A.bounds mirror
        columnSolutions = map (getColumnSolution mirror) [0..xEnd-1]

getRowSolutions mirror = sum $ map (*100) rowSolutions
  where (_, (xEnd, yEnd)) = A.bounds mirror
        rowSolutions = map (getRowSolution mirror) [0..yEnd-1]

getRowSolutions' mirror = map id rowSolutions
  where (_, (xEnd, yEnd)) = A.bounds mirror
        rowSolutions = map (getRowSolution mirror) [0..yEnd-1]

flips :: Mirror -> [Mirror]
flips mirror = [mirror A.// [((x,y), flip (x,y))] | x <- [xs..xEnd], y <- [ys..yEnd]]
  where
    ((xs, ys),(xEnd, yEnd)) = A.bounds mirror
    flip (x,y) = let el = mirror A.! (x,y) in if el == '.' then '#' else '.'


getAltSolutions mirror = fromJust $ find (/=existing) sols
  where existing = head $ getSolutions' mirror
        sols = concatMap getSolutions' $ flips mirror
        -- ans = 
        --   if cs /= 0 then map (\(cs',rs') -> if cs' == cs then rs' else cs') sols
        --              else map (\(cs',rs') -> if rs' == rs then cs' else rs') sols


getSolutions :: Mirror -> Int
getSolutions mirror = sum $ columnSolutions ++ map (*100) rowSolutions
  where (_, (xEnd, yEnd)) = A.bounds mirror
        columnSolutions = map (getColumnSolution mirror) [0..xEnd-1]
        rowSolutions = map (getRowSolution mirror) [0..yEnd-1]

getSolutions' :: Mirror -> [Int]
getSolutions' mirror = filter (/= 0) $ columnSolutions ++ map (*100) rowSolutions
  where (_, (xEnd, yEnd)) = A.bounds mirror
        columnSolutions = map (getColumnSolution mirror) [0..xEnd-1]
        rowSolutions = map (getRowSolution mirror) [0..yEnd-1]


mirrorToString m = unlines $ map (flip getRow m) [ys..ye]
  where
    ((xs, ys),(xe, ye)) = A.bounds m

weird = id
 ["#....#..#",
  "#....#..#",
  "..##..###",
  "#####.##.",
  "#####.##.",
  "..##..###",
  "#....#..#"]

getAnswerA = show . sum . map getSolutions .  parseInput

getAnswerB = show . sum . map getAltSolutions .  parseInput