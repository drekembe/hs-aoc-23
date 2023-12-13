module Day13 where

import qualified Data.Array as A
import Data.List.Split (splitOn)

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
getRow n mirror = map (\x -> mirror A.! (x,h)) [0..w]
  where (_, (w,h)) = A.bounds mirror

reflectionSize :: 


getAnswerA = getColumn 0 . head . parseInput
getAnswerB _ = 3