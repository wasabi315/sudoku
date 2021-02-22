{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Data.Foldable
import Sudoku qualified

main :: IO ()
main =
  do
    probs <- lines <$> getContents
    for_ probs \prob ->
      case Sudoku.parse prob of
        Nothing -> putStrLn "Invalid input"
        Just sudoku -> case Sudoku.solve sudoku of
          Nothing -> putStrLn "No solution found"
          Just sol -> putStrLn sol
