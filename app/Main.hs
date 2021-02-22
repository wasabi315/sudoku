{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Control.Arrow
import Control.Monad
import Data.Foldable
import Data.Maybe
import Sudoku qualified

main :: IO ()
main =
  do
    ps <- lines <$> getContents
    for_ ps $
      putStrLn
        <<< fromMaybe "No solution found"
        <<< Sudoku.solve
        <=< Sudoku.parse
