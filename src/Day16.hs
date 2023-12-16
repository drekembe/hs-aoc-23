module Day16 where

import Data.Array
import Control.Monad (guard)
import Control.Applicative (empty)

type Grid = Array (Int, Int) Char
data Dir = U | D | L | R deriving (Show, Eq)
data Pos = Pos (Int, Int) Dir deriving (Show, Eq)
type History = [(Int, Int)]

sample = unlines [
  ".|...\\....",
  "|.-.\\.....",
  ".....|-...",
  "........|.",
  "..........",
  ".........\\",
  "..../.\\\\..",
  ".-.-/..|..",
  ".|....-|.\\",
  "..//.|...."
  ]

parseInput :: String -> Grid
parseInput input = array ((0, 0), (xe, ye)) (zip [(x, y) | y <- [0 .. ye], x <- [0 .. xe]] $ concat (lines input))
  where
    xe = (length . head . lines $ input) - 1
    ye = length (lines input) - 1

initial = (Pos (-1,0) R, '.', [])

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

move :: Grid -> Pos -> History -> [(Pos, History)]
move grid (Pos (x,y) dir) history = do
  let c = if inBounds grid (x,y) then grid ! (x,y) else '.'
  (Pos (newX, newY) newDir) <- go c (Pos (x,y) dir)
  guard $ inBounds grid (newX, newY)
  return (Pos (newX, newY) newDir, (newX, newY):history)


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

getAnswerA = show . parseInput

getAnswerB _ = "lmao"