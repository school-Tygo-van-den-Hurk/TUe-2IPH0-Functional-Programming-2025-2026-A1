module Main (main) where

import FormulaManipulator (processCLIArgs)
import System.Environment (getArgs)

main :: IO ()
main = do
  as <- getArgs
  putStrLn (processCLIArgs as)
