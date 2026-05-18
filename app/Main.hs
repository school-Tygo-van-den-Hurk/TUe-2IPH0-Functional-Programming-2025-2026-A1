module Main (main) where

import System.Environment (getArgs)
import FormulaManipulator (processCLIArgs)

main :: IO ()
main = do
        as <- getArgs
        putStrLn (processCLIArgs as)
