module Day15 where

import Data.List.Split (splitOn)
import Data.List (find)
import qualified Data.Map as M
import Data.Char (ord)
import Data.Maybe (fromJust)

type Boxes = M.Map Int [Lens]
type Lens = (String, Int)

data Instruction = Add String Int | Remove String deriving (Show, Eq)

hash :: String -> Int
hash = foldl (\acc s -> ((acc + ord s) * 17) `mod` 256) 0

parseInstruction :: String -> Instruction
parseInstruction instr
  | '=' `elem` instr = let [s,i] = splitOn "=" instr in Add s (read i)
  | otherwise = Remove $ take (length instr - 1) instr

step :: Boxes -> Instruction -> Boxes
step bs (Add label power) = update present
  where present = M.lookup (hash label) bs >>= find ((==label) . fst)
        update (Just _) = M.adjust (map switchLabel) (hash label) bs
        update Nothing = M.insert (hash label) (M.findWithDefault [] (hash label) bs ++ [(label, power)]) bs
        switchLabel (l,p) = if label == l then (label, power) else (l,p)
step bs (Remove label) = M.adjust (filter (\(label',_) -> label /= label')) (hash label) bs

lp :: [Lens] -> Int
lp = sum . zipWith p [1..]
  where p i (label,power) = (1 + hash label) * i * power

getAnswerA :: String -> String
getAnswerA = show . sum . map hash  . splitOn ","
getAnswerB = show . sum . map (lp . snd) . M.toList . foldl step M.empty . map parseInstruction . splitOn ","