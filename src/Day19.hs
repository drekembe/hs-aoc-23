module Day19 where

import Data.List.Split (splitOn)
import Data.List (find)
import qualified Data.Map as M

data Item = Item {x :: Int, m :: Int, a :: Int, s :: Int} deriving (Show, Eq, Ord)

data Workflow = Workflow { label :: String, instructions :: [WorkflowInstruction], noMatch :: Target }

data Target = Target String | Accept | Reject deriving (Show, Eq)

data WorkflowInstruction = WorkflowInstruction
  { predicate :: Item -> Bool,
    target :: Target
  }

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
    { predicate = \item -> parseOp op (parseL l item) (read num),
      target = parseTarget tl
    }
  where
    [num, tl] = splitOn ":" rest
    parseL 'x' = x
    parseL 'm' = m
    parseL 'a' = a
    parseL 's' = s
    parseL _ = undefined
    parseOp '>' = (>)
    parseOp '<' = (<)
    parseOp _ = undefined

parseInput :: String -> (M.Map String Workflow, [Item])
parseInput input = (workflowMap $ map parseWorkflow wfs, map parseItem items)
  where
    [wfs, items] = splitOn [""] . lines $ input

workflowMap :: [Workflow] -> M.Map String Workflow
workflowMap = foldr (\wf m -> M.insert (label wf) wf m) M.empty

processWorkflowMap :: M.Map String Workflow -> Item -> Target
processWorkflowMap map item = go "in"
  where go label = case M.lookup label map of Nothing -> error "not found"
                                              (Just workflow) -> case processItem workflow item of Reject -> Reject
                                                                                                   Accept -> Accept
                                                                                                   Target l -> go l


processItem :: Workflow -> Item -> Target
processItem (Workflow { label=l, instructions=ins, noMatch=nm }) item =
  case find (`predicate` item) ins of Nothing -> nm
                                      (Just wf) -> target wf

processInput mm = sum . map (\i -> x i + m i + a i + s i ) . filter (\item -> processWorkflowMap mm item == Accept)

getAnswerA = show . uncurry processInput . parseInput 

getAnswerB _ = "OK"