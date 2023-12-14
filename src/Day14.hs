module Day14 where

import Data.Array (Array, array, bounds, (!), (//))
import Data.List (groupBy, sortBy)
import Data.List.Split (splitWhen)
import qualified Data.Ord
import qualified Data.Set as S
import Util (getColumn, rotateCw)

type Field = Array (Int, Int) Char

parseInput :: String -> Field
parseInput input =
  array ((0, 0), (xMax, yMax)) . zip [(x, y) | y <- [0 .. yMax], x <- [0 .. xMax]] . concat $ ls
  where
    ls = lines input
    xMax = length (head ls) - 1
    yMax = length ls - 1

bunchUp :: String -> String
bunchUp =
  concatMap (\(s : ss) -> if s == '#' then s : ss else sortBy (Data.Ord.comparing Data.Ord.Down) (s : ss))
    . groupBy (\a b -> [a, b] == "##" || '#' `notElem` [a, b])

moveNorth :: Field -> Field
moveNorth field = foldl (\arr c -> arr // zip [(c, y) | y <- [ys .. ye]] (bunchUp [arr ! (c, y) | y <- [ys .. ye]])) field [xs .. xe]
  where
    ((xs, ys), (xe, ye)) = bounds field

cycleField :: Field -> Field
cycleField = rotateCw . moveNorth . rotateCw . moveNorth . rotateCw . moveNorth . rotateCw . moveNorth

cycleFields :: S.Set Field -> Field -> Int -> [(S.Set Field, Field, Int, Int)]
cycleFields seen field cycles = (seen, field, scoreField field, cycles) : next
  where
    newField = cycleField field
    newSeen = S.insert field seen
    next = if field `elem` seen then [] else cycleFields newSeen newField (cycles + 1)

scoreField :: Field -> Int
scoreField field = sum $ map (\x -> scoreColumn $ getColumn x field) [xs .. xe]
  where
    ((xs, ys), (xe, ye)) = bounds field
    scoreColumn x = sum $ zipWith (\s ch -> if ch == 'O' then s else 0) [length x, length x - 1 ..] x

cycleUntilRepeat :: Field -> [(Int,Int)]
cycleUntilRepeat =
  map (\(s, f, score, cycle) -> (S.size s, score))
    . (\f -> cycleFields S.empty f 0)

getAnswerA = show . scoreField . moveNorth . parseInput

getAnswerB inp = show . snd $ results !! (1000000000 - lastBeforeBillion + cycleStartsAt)
  where results = cycleUntilRepeat (parseInput inp)
        firstRepeat = snd (last results)
        cycleStartsAt = length $ takeWhile ((/=firstRepeat) . snd) results
        cycleLength = length (drop cycleStartsAt results) - 1
        lastBeforeBillion = head . dropWhile (<1000000000 - cycleLength) $ [cycleStartsAt, cycleStartsAt+cycleLength ..]