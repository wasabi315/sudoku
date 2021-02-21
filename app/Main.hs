module Main where

import Control.Arrow
import Control.Monad
import Data.Foldable
import Data.Maybe
import Sudoku

main :: IO ()
main =
  do
    ps <- lines <$> getContents
    for_ ps $ \p -> fromJust (readSudoku p) `seq` pure ()

-- for_ ps $ (readSudoku >=> solveSudoku) >>> traverse_ putStrLn
