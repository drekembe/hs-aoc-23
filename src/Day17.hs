module Day17 where

import Control.Monad (when)
import Control.Monad.State.Lazy
import Data.Array
import qualified Data.Heap as H
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Util (inBounds)

type Heat = Int

type Vertex = (Int, Int, Int, Int, Int)

type Grid = Array (Int, Int) Int

type PQ = H.Heap (Heat, Vertex)

type Visited = S.Set Vertex

parseInput :: String -> Grid
parseInput input =
  array ((xs, ys), (xe, ye))
    . zipWith (\(x, y) n -> ((x, y), read [n])) [(x, y) | y <- [ys .. ye], x <- [xs .. xe]]
    . concat
    . lines
    $ input
  where
    ls = lines input
    (ys, ye) = (0, length ls - 1)
    (xs, xe) = (0, length (head ls) - 1)

shortestPath :: Grid -> Heat
shortestPath grid = fst . fst . fromJust . H.viewMin . fst $ execState (go2 ()) (H.singleton (0, (0, 0, 0, 0, 0)), S.empty)
  where
    ((xs, ys), (xe, ye)) = bounds grid
    go2 :: () -> State (PQ, Visited) ()
    go2 () = do
      (pq, visited) <- get
      let ((heat, v@(x, y, dx, dy, n)), pq') = fromJust $ H.viewMin pq
      put (pq', S.insert v visited)
      if x == xe && y == ye
        then return ()
        else
          if v `elem` visited
            then go2 ()
            else do
              when (n < 3 && (dx, dy) /= (0, 0) && inBounds grid (x + dx, y + dy)) $ modify (\(pq, v) -> (H.insert (heat + grid ! (x + dx, y + dy), (x + dx, y + dy, dx, dy, n + 1)) pq, v))
              mapM_ (\(ndx, ndy) -> when ((ndx, ndy) /= (dx, dy) && (ndx, ndy) /= (-dx, -dy) && inBounds grid (x + ndx, y + ndy)) $ modify (\(pq, v) -> (H.insert (heat + grid ! (x + ndx, y + ndy), (x + ndx, y + ndy, ndx, ndy, 1)) pq, v))) [(0, 1), (0, -1), (1, 0), (-1, 0)]
              go2 ()

getAnswerA = show . shortestPath . parseInput

getAnswerB _ = "OK"