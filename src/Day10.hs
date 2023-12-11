module Day10 where

import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S

type Coord = (Integer, Integer)

type PipeMap = M.Map Coord Char

data Direction = CW | CCW deriving (Eq, Show)

data Orientation = L | R | U | D deriving (Eq, Show, Ord)

parseInput :: String -> PipeMap
parseInput input =
  let ls = lines input
      coords = concatMap ((\a b -> map (\x -> (x, b)) a) (take (length $ head ls) [0 ..])) [0 ..]
   in M.fromList $ zipWith (\(x, y) c -> ((x, y), c)) coords (concat ls)

startingPoint :: PipeMap -> Coord
startingPoint = fst . fromJust . find (\(coord, char) -> char == 'S') . M.toList

up (x, y) = (x, y - 1)

down (x, y) = (x, y + 1)

left (x, y) = (x - 1, y)

right (x, y) = (x + 1, y)

getInitialOrientations :: PipeMap -> (Orientation, Orientation)
getInitialOrientations pm =
  let sp = startingPoint pm
      u = M.findWithDefault '.' (up sp) pm
      d = M.findWithDefault '.' (down sp) pm
      l = M.findWithDefault '.' (left sp) pm
      r = M.findWithDefault '.' (right sp) pm
      valid = ([U | u `elem` "F7|"]) ++ ([D | d `elem` "|JL"]) ++ ([R | r `elem` "J7-"]) ++ ([L | l `elem` "LF-"])
   in (head valid, valid !! 1)

nextStep :: PipeMap -> (Orientation, Coord) -> (Orientation, Coord)
nextStep pm (ori, coord) =
  let ch = fromJust $ M.lookup coord pm
      go '-' L = L
      go '-' R = R
      go '|' U = U
      go '|' D = D
      go 'F' U = R
      go 'F' L = D
      go '7' R = D
      go '7' U = L
      go 'J' D = L
      go 'J' R = U
      go 'L' D = R
      go 'L' L = U
      go 'S' o = let (a, b) = getInitialOrientations pm in if o == a then a else b
      go _ _ = undefined
      dirf L = left
      dirf R = right
      dirf D = down
      dirf U = up
   in (go ch ori, dirf (go ch ori) coord)

startingPointCharacter pm =
  let a : b : _ = sort [fst $ getInitialOrientations pm, snd $ getInitialOrientations pm]
      go L R = '-'
      go U D = '|'
      go R D = 'F'
      go L D = '7'
      go R U = 'L'
      go L U = 'J'
   in go a b

path :: PipeMap -> [(Orientation, Coord)]
path pm =
  let sp = startingPoint pm
      o = fst $ getInitialOrientations pm
   in (o, sp) : (takeWhile (\(o, c) -> c /= sp) . drop 1 $ iterate (nextStep pm) (o, sp))

pointsInPath :: PipeMap -> S.Set Coord
pointsInPath = S.fromList . map snd . path

isInside :: PipeMap -> Coord -> Bool
isInside pm (x, y) =
  let includeS = startingPointCharacter pm `elem` ['-', 'L', 'J']
      notWallChars = ['-', 'L', 'J'] ++ (['S' | includeS])
      inPath = pointsInPath pm
      pathOk =
        odd
          . length
          . filter (\(_, (x', y')) -> (`notElem` notWallChars) . fromJust . M.lookup (x', y') $ pm)
          . filter (\(_, (x', y')) -> x' < x)
          . filter (\(_, (x', y')) -> y == y')
          $ path pm
   in pathOk && S.notMember (x,y) inPath

numberOfPointsInside :: PipeMap -> Int
numberOfPointsInside pm = M.size . M.filterWithKey (\coord _ -> isInside pm coord) $ pm

getAnswerA = (`div` 2) . length . path . parseInput

getAnswerB = numberOfPointsInside . parseInput