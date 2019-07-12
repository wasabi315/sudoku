module Main where

import           Data.Foldable

import           Sudoku

-------------------------------------------------------------------------------

main :: IO ()
main = do
    ps <- lines <$> getContents
    for_ ps $ traverse_ putStrLn . solveSudoku

