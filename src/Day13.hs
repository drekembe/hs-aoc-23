module Day13 where

import qualified Data.Array as A
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import Data.Foldable (toList)
import Data.List

type Mirror = A.Array (Int, Int) Char

data Orientation = Row | Column deriving (Show, Eq)

parseInput :: String -> [Mirror]
parseInput = map parseMirror . splitOn [""] . lines

parseMirror :: [String] -> Mirror
parseMirror lines = A.array ((0,0), (length (head lines) - 1 , length lines - 1))
  . concat
  . zipWith (\y line -> zipWith (\x ch -> ((x,y), ch)) [0..] line) [0..]
  $ lines

getColumn :: Int -> Mirror -> String
getColumn n mirror = map (\x -> mirror A.! (n,x)) [0..h]
  where (_, (w,h)) = A.bounds mirror

getRow :: Int -> Mirror -> String
getRow n mirror = map (\x -> mirror A.! (x,n)) [0..w]
  where (_, (w,h)) = A.bounds mirror

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

flips :: Mirror -> [Mirror]
flips mirror = [mirror A.// [((x,y), flip (x,y))] | x <- [xEnd, xEnd-1 .. 0], y <- [yEnd,yEnd-1..0]]
  where
    (_,(xEnd, yEnd)) = A.bounds mirror
    flip (x,y) = let el = mirror A.! (x,y) in if el == '.' then '#' else '.'


getAltSolutions mirror = sum $ toList (find (/= 0) ans)
  where cs = getColumnSolutions mirror
        rs = getRowSolutions mirror
        sols = map (\m -> (getColumnSolutions m, getRowSolutions m)) $ flips mirror
        ans = 
          if cs /= 0 then map (\(cs',rs') -> if cs' == cs then rs' else cs') sols
                     else map (\(cs',rs') -> if rs' == rs then cs' else rs') sols
          -- find (/= 0) 
          -- . map (uncurry (+))
          -- . filter (\(_, newRs) -> newRs /= rs)
          -- . filter (\(newCs, _) -> newCs /= 0 && newCs /= cs)
          --  filter (\(cs', rs') -> if cs == 0 then rs' /= rs else cs' /= cs) .
          --  map (\m -> (getColumnSolutions m, getRowSolutions m))
          -- $ flips mirror


getSolutions :: Mirror -> Int
getSolutions mirror = sum $ columnSolutions ++ map (*100) rowSolutions
  where (_, (xEnd, yEnd)) = A.bounds mirror
        columnSolutions = map (getColumnSolution mirror) [0..xEnd-1]
        rowSolutions = map (getRowSolution mirror) [0..yEnd-1]


getAnswerA = sum . map getSolutions . parseInput
getAnswerB _ = 3