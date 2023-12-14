module Util (getColumn, getRow, rotateCcw, rotateCw, applyToCol) where
import Data.Array


getColumn c field = map (\y -> field ! (c,y)) [ys..ye]
  where ((_,ys),(_,ye)) = bounds field

getRow r field = map (\x -> field ! (x,r)) [xs..xe]
  where ((xs,_),(xe,_)) = bounds field


rotateCcw arr = ixmap ((ys,xs),(ye,xe)) (\(x,y) -> (xe-y,x)) arr
  where ((xs,ys),(xe,ye)) = bounds arr

rotateCw arr = ixmap ((ys,xs),(ye,xe)) (\(x,y) -> (y,ye-x)) arr
  where ((xs,ys),(xe,ye)) = bounds arr

applyToCol n f arr = arr // zip [(n,y) | y <- [ys..ye]] (f (getColumn n arr))
  where ((xs,ys),(xe,ye)) = bounds arr