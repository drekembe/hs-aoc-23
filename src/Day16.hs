module Day16 where

import Data.Array
import Control.Monad (guard)
import qualified Data.Set as S
import qualified Data.Map as M
import Util (inBounds)

type Grid = Array (Int, Int) Char
data Dir = U | D | L | R deriving (Show, Eq, Ord)
data Pos = Pos (Int, Int) Dir deriving (Show, Eq, Ord)
type History = [Pos]

parseInput :: String -> Grid
parseInput input = array ((0, 0), (xe, ye)) (zip [(x, y) | y <- [0 .. ye], x <- [0 .. xe]] $ concat (lines input))
  where
    xe = (length . head . lines $ input) - 1
    ye = length (lines input) - 1

energize :: Pos -> Grid -> Int
energize pos grid = go (S.singleton pos) [pos]
  where
    go :: S.Set Pos -> [Pos] -> Int
    go visited [] = length $ S.map (\(Pos (x,y) _) -> (x,y)) visited
    go visited beams = go (visited `S.union` S.fromList nb) nb
      where nb = concatMap (filter (`S.notMember` visited) . filter (inGrid grid) . (\p -> step (charAt p grid) p)) beams
            charAt (Pos (x,y) dir) grid = grid ! (x,y)
            inGrid grid (Pos p _) = inBounds grid p

step '.' p = [continue p]
step '/' p@(Pos _ R) = [up p]
step '/' p@(Pos _ D) = [left p]
step '/' p@(Pos _ L) = [down p]
step '/' p@(Pos _ U) = [right p]
step '\\' p@(Pos _ R) = [down p]
step '\\' p@(Pos _ D) = [right p]
step '\\' p@(Pos _ L) = [up p]
step '\\' p@(Pos _ U) = [left p]
step '-' p@(Pos _ R) = [continue p]
step '-' p@(Pos _ L) = [continue p]
step '-' p@(Pos _ U) = [left p, right p]
step '-' p@(Pos _ D) = [left p, right p]
step '|' p@(Pos _ R) = [up p, down p]
step '|' p@(Pos _ L) = [up p, down p]
step '|' p@(Pos _ U) = [continue p]
step '|' p@(Pos _ D) = [continue p]
step _ _ = error "unexpected step"

down (Pos (x,y) _) = Pos (x,y+1) D
up (Pos (x,y) _) = Pos (x,y-1) U
left (Pos (x,y) _) = Pos (x-1,y) L
right (Pos (x,y) _) = Pos (x+1,y) R
continue p@(Pos _ R) = right p
continue p@(Pos _ D) = down p
continue p@(Pos _ L) = left p
continue p@(Pos _ U) = up p

allStartingPositions :: Grid -> [Pos]
allStartingPositions grid =
  map (\x -> Pos (x,ys) D) [xs..xe] ++
  map (\x -> Pos (x,ye) U) [xs..xe] ++
  map (\y -> Pos (xs,y) R) [ys..ye] ++
  map (\y -> Pos (xe,y) L) [ys..ye]
  where ((xs,ys), (xe,ye)) = bounds grid

getAnswerA = show . energize (Pos (0,0) R) . parseInput

getAnswerB input = show . maximum . map (`energize` grid)  . allStartingPositions $ grid
  where grid = parseInput input