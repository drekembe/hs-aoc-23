module Main where
import System.Environment
import Day17 (getAnswerA, getAnswerB)

main :: IO ()
main = do
  fileContents <- getArgs >>= readFile . head
  -- putStrLn $ "Answer A: " ++ show (getAnswerA fileContents)
  putStrLn "[Answer A]"
  putStrLn $ getAnswerA fileContents
  putStrLn "[Answer B]"
  putStrLn $ getAnswerB fileContents
