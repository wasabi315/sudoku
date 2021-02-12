module Main where

import Control.Arrow
import Control.Monad
import Data.Foldable

import Sudoku

main :: IO ()
main =
    do
        ps <- lines <$> getContents
        for_ ps $ (readSudoku >=> solveSudoku) >>> traverse_ putStrLn
