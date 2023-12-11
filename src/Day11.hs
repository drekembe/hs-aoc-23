module Day11 where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

type Star = (Int, Int)

data StarField = StarField {stars :: S.Set Star, width :: Int, height :: Int} deriving (Show, Eq)

parseInput :: String -> StarField
parseInput input =
  let ls = lines input
      lineLength = length $ head ls
      coordinates = flip (,) <$> [0 .. lineLength - 1] <*> [0 .. length ls - 1]
   in StarField
        { stars = S.fromList . map fst . filter ((== '#') . snd) . zip coordinates . concat $ ls,
          width = lineLength,
          height = length ls
        }

emptyColumns :: StarField -> [Int]
emptyColumns StarField {stars = stars, width = width} = 
  filter (\x -> S.null $ S.filter (\(x', y') -> x' == x) stars) [0 .. width - 1]

emptyRows :: StarField -> [Int]
emptyRows StarField {stars = stars, height = height} = 
  filter (\y -> S.null $ S.filter (\(x', y') -> y' == y) stars) [0 .. height - 1]

expandStar :: StarField -> Int -> Star -> Star
expandStar sf by (x, y) =
  let emptyCs = emptyColumns sf
      emptyRs = emptyRows sf
   in (x + (sum . map (\_ -> by - 1) . filter (< x) $ emptyCs), y + (sum . map (\_ -> by - 1) . filter (< y) $ emptyRs))

expand :: Int -> StarField -> StarField
expand by sf = StarField {stars = S.map (expandStar sf by) (stars sf), width = width sf, height = height sf}

distance (x, y) (x', y') = abs (x - x') + abs (y - y')

allPairs :: StarField -> [(Star, Star)]
allPairs sf = [(x, y) | (x : ys) <- tails . S.toList . stars $ sf, y <- ys]

getAnswerA = sum . map (uncurry distance) . allPairs . expand 2 . parseInput

getAnswerB = sum . map (uncurry distance) . allPairs . expand 1000000 . parseInput
