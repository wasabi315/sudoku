{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Control.Arrow
import Control.Monad
import Data.Foldable
import Sudoku qualified

main :: IO ()
main =
  do
    ps <- lines <$> getContents
    for_ ps $ traverse_ putStrLn <<< Sudoku.solve <=< Sudoku.parse
