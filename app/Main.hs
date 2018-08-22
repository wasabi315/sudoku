module Main where

import Data.Char   ( digitToInt )
import Data.Either

import Sudoku

main :: IO ()
main = do
    str <- parseSudokuFromFile "./sudoku.csv"
    either print execSolver str

execSolver :: String -> IO ()
execSolver str = do
    let b = toBoard str
    putStrLn "target:"
    printBoard b
    putStrLn "solutions:"
    mapM_ printBoard $ solver b

