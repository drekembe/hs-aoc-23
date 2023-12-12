module Main where
import System.Environment
import Day12 (getAnswerA, getAnswerB)

main :: IO ()
main = do
  fileContents <- getArgs >>= readFile . head
  putStrLn $ "Answer A: " ++ show (getAnswerA fileContents)
  putStrLn $ "Answer B: " ++ show (getAnswerB fileContents)