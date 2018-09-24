module Main where

import           Data.Char   ( digitToInt )
import           Data.Either ( either )

import           Sudoku

main :: IO ()
main = do
    str <- parseSudokuFromFile "./sudoku.csv"
    either print execSolver str

execSolver :: String -> IO ()
execSolver = mapM_ printBoard . solver . toBoard

