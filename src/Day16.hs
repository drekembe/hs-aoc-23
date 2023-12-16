module Day16 where

import Data.Array
import Control.Monad (guard)
import qualified Data.Set as S
import qualified Data.Map as M

type Grid = Array (Int, Int) Char
data Dir = U | D | L | R deriving (Show, Eq, Ord)
data Pos = Pos (Int, Int) Dir deriving (Show, Eq, Ord)
type History = [Pos]

parseInput :: String -> Grid
parseInput input = array ((0, 0), (xe, ye)) (zip [(x, y) | y <- [0 .. ye], x <- [0 .. xe]] $ concat (lines input))
  where
    xe = (length . head . lines $ input) - 1
    ye = length (lines input) - 1

down (Pos (x,y) _) = Pos (x,y+1) D
up (Pos (x,y) _) = Pos (x,y-1) U
left (Pos (x,y) _) = Pos (x-1,y) L
right (Pos (x,y) _) = Pos (x+1,y) R
continue p@(Pos _ R) = right p
continue p@(Pos _ D) = down p
continue p@(Pos _ L) = left p
continue p@(Pos _ U) = up p

inBounds arr (x,y) = (x >= xs) && (x <= xe) && (y >= ys) && (y <= ye)
  where ((xs,ys), (xe,ye)) = bounds arr

shootBeam :: Pos -> Grid -> Int
shootBeam pos grid = shootBeam' (S.singleton pos) [pos]
  where
    shootBeam' :: S.Set Pos -> [Pos] -> Int
    shootBeam' visited [] = length $ S.map (\(Pos (x,y) _) -> (x,y)) visited
    shootBeam' visited beams = shootBeam' (visited `S.union` S.fromList nb) nb
      where nb = concatMap (filter (`S.notMember` visited) . filter (inGrid grid) . (\p -> go (charAt p grid) p)) beams
            charAt (Pos (x,y) dir) grid = grid ! (x,y)
            inGrid grid (Pos p _) = inBounds grid p

allStartingPositions :: Grid -> [Pos]
allStartingPositions grid =
  map (\x -> Pos (x,ys) D) [xs..xe] ++
  map (\x -> Pos (x,ye) U) [xs..xe] ++
  map (\y -> Pos (xs,y) R) [ys..ye] ++
  map (\y -> Pos (xe,y) L) [ys..ye]
  where ((xs,ys), (xe,ye)) = bounds grid


go '.' p = [continue p]
go '/' p@(Pos _ R) = [up p]
go '/' p@(Pos _ D) = [left p]
go '/' p@(Pos _ L) = [down p]
go '/' p@(Pos _ U) = [right p]
go '\\' p@(Pos _ R) = [down p]
go '\\' p@(Pos _ D) = [right p]
go '\\' p@(Pos _ L) = [up p]
go '\\' p@(Pos _ U) = [left p]
go '-' p@(Pos _ R) = [continue p]
go '-' p@(Pos _ L) = [continue p]
go '-' p@(Pos _ U) = [left p, right p]
go '-' p@(Pos _ D) = [left p, right p]
go '|' p@(Pos _ R) = [up p, down p]
go '|' p@(Pos _ L) = [up p, down p]
go '|' p@(Pos _ U) = [continue p]
go '|' p@(Pos _ D) = [continue p]
go _ _ = error "unexpected go"

getAnswerA = show . shootBeam (Pos (0,0) R) . parseInput

getAnswerB input = show . maximum . map (`shootBeam` grid)  . allStartingPositions $ grid
  where grid = parseInput input