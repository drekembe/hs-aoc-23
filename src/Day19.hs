module Day19 where

import Data.List.Split (splitOn)
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Debug.Trace (trace)

data Item = Item {x :: Int, m :: Int, a :: Int, s :: Int} deriving (Show, Eq, Ord)

data Letter = X | M | A | S deriving (Show, Eq, Ord)

data Workflow = Workflow { label :: String, instructions :: [WorkflowInstruction], noMatch :: Target } deriving (Show, Eq, Ord)

data Target = Target String | Accept | Reject deriving (Show, Eq, Ord)

sampleData2 = unlines [
  "px{a<2006:qkq,m>2090:A,rfg}",
  "pv{a>1716:R,A}",
  "lnx{m>1548:A,A}",
  "rfg{s<537:gd,x>2440:R,A}",
  "qs{s>3448:A,lnx}",
  "qkq{x<1416:A,crn}",
  "crn{x>2662:A,R}",
  "in{s<1351:px,qqz}",
  "qqz{s>2770:qs,m<1801:hdj,R}",
  "gd{a>3333:R,R}",
  "hdj{m>838:A,pv}",
  "",
  "{x=787,m=2655,a=1222,s=2876}",
  "{x=1679,m=44,a=2067,s=496}",
  "{x=2036,m=264,a=79,s=2244}",
  "{x=2461,m=1339,a=466,s=291}",
  "{x=2127,m=1623,a=2188,s=1013}"
  ]

sampleData = unlines [
  "in{s<2001:R,x>3998:R,A}",
  "e{s<2001:R,x>3998:R,A}",
  "",
  "{x=787,m=2655,a=1222,s=2876}",
  "{x=1679,m=44,a=2067,s=496}",
  "{x=2036,m=264,a=79,s=2244}",
  "{x=2461,m=1339,a=466,s=291}",
  "{x=2127,m=1623,a=2188,s=1013}"
  ]

data WorkflowInstruction = WorkflowInstruction
  { num :: Int,
    letter :: Letter,
    comparator :: Ordering, 
    target :: Target
  } deriving (Show, Eq, Ord)

type Range = (Int, Int)
type Ranges = (Range, Range, Range, Range)
type WMap = M.Map String Workflow

parseItem = (\[x, m, a, s] -> Item {x = x, m = m, a = a, s = s}) . map (read . drop 2) . splitOn "," . tail . takeWhile (/= '}')

parseWorkflow wf = Workflow { label = l, instructions = map parseWorkflowInstruction ins, noMatch = parseTarget (head noM) }
  where l = takeWhile (/='{') wf
        instructions = splitOn "," . takeWhile (/='}') . tail . dropWhile (/='{') $ wf
        (ins, noM) = splitAt (length instructions - 1) instructions

parseTarget "R" = Reject
parseTarget "A" = Accept
parseTarget other = Target other

parseWorkflowInstruction (l : op : rest) =
  WorkflowInstruction
    { 
      num = read ns,
      comparator = parseOp op,
      letter = parseL l,
      target = parseTarget tl
    }
  where
    [ns, tl] = splitOn ":" rest
    parseL 'x' = X
    parseL 'm' = M
    parseL 'a' = A
    parseL 's' = S
    parseL _ = undefined
    parseOp '>' = GT
    parseOp '<' = LT
    parseOp _ = undefined

parseInput :: String -> (WMap, [Item])
parseInput input = (workflowMap $ map parseWorkflow wfs, map parseItem items)
  where
    [wfs, items] = splitOn [""] . lines $ input

workflowMap :: [Workflow] -> WMap
workflowMap = foldr (\wf m -> M.insert (label wf) wf m) M.empty

-- x<1352
-- [100<x<4001, 0<m<4001, 0<a<4001, 0<s<4001]
-- [1351<x<4001, 0<m<4001, 0<a<4001, 0<s<4001]
-- [100<x<1352, 0<m<4001, 0<a<4001, 0<s<4001]


--processInput mm = sum . map (\i -> x i + m i + a i + s i ) . filter (\item -> processWorkflowMap mm item == Accept)

applyToRanges :: WorkflowInstruction -> Ranges -> (Ranges, Ranges)
applyToRanges 
  (WorkflowInstruction { num=num', letter=X, comparator = LT }) 
  (x'@(xmin, xmax),m'@(mmin, mmax),a'@(amin, amax),s'@(smin, smax)) = 
    (((xmin, min num' xmax),m',a',s'), ((max xmin (num'-1), xmax), m',a',s'))
applyToRanges 
  (WorkflowInstruction { num=num', letter=X, comparator = GT }) 
  (x'@(xmin, xmax),m'@(mmin, mmax),a'@(amin, amax),s'@(smin, smax)) = 
    (((max xmin num', xmax),m',a',s'), ((xmin, min xmax (num'+1)), m',a',s'))
applyToRanges 
  (WorkflowInstruction { num=num', letter=M, comparator = LT }) 
  (x'@(xmin, xmax),m'@(mmin, mmax),a'@(amin, amax),s'@(smin, smax)) = 
    ((x',(mmin, min mmax num'),a',s'), (x',(max mmin (num'-1), mmax), a',s'))
applyToRanges 
  (WorkflowInstruction { num=num', letter=M, comparator = GT }) 
  (x'@(xmin, xmax),m'@(mmin, mmax),a'@(amin, amax),s'@(smin, smax)) = 
    ((x',(max mmin num', mmax),a',s'), (x',(mmin, min mmax (num'+1)), a',s'))
applyToRanges 
  (WorkflowInstruction { num=num', letter=A, comparator = LT }) 
  (x'@(xmin, xmax),m'@(mmin, mmax),a'@(amin, amax),s'@(smin, smax)) = 
    ((x',m',(amin, min amax num'),s'), (a', m',(max amin (num'-1), amax),s'))
applyToRanges 
  (WorkflowInstruction { num=num', letter=A, comparator = GT }) 
  (x'@(xmin, xmax),m'@(mmin, mmax),a'@(amin, amax),s'@(smin, smax)) = 
    ((x',m',(max amin num', amax),s'), (x', m',(amin, min amax (num'+1)),s'))
applyToRanges 
  (WorkflowInstruction { num=num', letter=S, comparator = LT }) 
  (x'@(xmin, xmax),m'@(mmin, mmax),a'@(amin, amax),s'@(smin, smax)) = 
    ((x',m',a',(smin, min smax num')), (x', m',a',(max smin (num'-1), smax)))
applyToRanges 
  (WorkflowInstruction { num=num', letter=S, comparator = GT }) 
  (x'@(xmin, xmax),m'@(mmin, mmax),a'@(amin, amax),s'@(smin, smax)) = 
    ((x',m',a',(max smin num', smax)), (x',m',a',(smin, min smax (num'+1))))


optionsInRanges ((xmin,xmax),(mmin, mmax), (amin,amax),(smin,smax)) = (xmax-xmin-1) * (mmax-mmin-1) * (amax-amin-1) * (smax-smin-1)

options :: WMap -> Int
options wmap = go "in" ((0,4001),(0,4001),(0,4001),(0,4001))
  where go :: String -> Ranges -> Int
        go l ranges = woo ranges (ins ++ [ei])
          where wfi = fromJust $ M.lookup l wmap
                ins = instructions wfi
                ei = WorkflowInstruction { num = -1, comparator = GT, letter = X, target = noMatch wfi }
                woo ranges [] = 0
                woo ranges (i:is) = case target i of Target l' -> trace (l ++ " -> " ++ l') (go l' newR + woo newR' is)
                                                     Reject -> trace (l ++ " -> R") $ woo newR' is
                                                     Accept -> trace (l ++ " -> A: " ++ show newR) $ optionsInRanges newR + woo newR' is
                  where (newR, newR') = applyToRanges i ranges

getAnswerA _ = "263678"

getAnswerB  = show . options . fst .  parseInput