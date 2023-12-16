module Util (getColumn, getRow, rotateCcw, rotateCw, applyToCol, inBounds) where
import Data.Array


getColumn c arr = map (\y -> arr ! (c,y)) [ys..ye]
  where ((_,ys),(_,ye)) = bounds arr

getRow r arr = map (\x -> arr ! (x,r)) [xs..xe]
  where ((xs,_),(xe,_)) = bounds arr


rotateCcw arr = ixmap ((ys,xs),(ye,xe)) (\(x,y) -> (xe-y,x)) arr
  where ((xs,ys),(xe,ye)) = bounds arr

rotateCw arr = ixmap ((ys,xs),(ye,xe)) (\(x,y) -> (y,ye-x)) arr
  where ((xs,ys),(xe,ye)) = bounds arr

applyToCol n f arr = arr // zip [(n,y) | y <- [ys..ye]] (f (getColumn n arr))
  where ((xs,ys),(xe,ye)) = bounds arr

inBounds arr (x,y) = (x >= xs) && (x <= xe) && (y >= ys) && (y <= ye)
  where ((xs,ys), (xe,ye)) = bounds arr