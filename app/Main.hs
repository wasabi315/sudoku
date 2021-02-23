{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Data.Foldable
import Data.Maybe
import Sudoku qualified

main :: IO ()
main =
  do
    inputs <- lines <$> getContents
    for_ inputs \input ->
      fromJust (Sudoku.parse input) `seq` pure ()

-- case Sudoku.parse input of
--   Nothing -> putStrLn "Invalid input"
--   Just sudoku -> case Sudoku.solve sudoku of
--     Nothing -> putStrLn "No solution found"
--     Just sol -> putStrLn sol
