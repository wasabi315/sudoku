module Main where

import           Control.Monad ( forM_ )
import           Data.Char   ( digitToInt )
import           Data.List   ( lines )

import           Sudoku

main :: IO ()
main = do
    inputs <- lines <$> getContents
    forM_ inputs $ \input ->
        maybe (print "Invalid Input") execSolver (toBoard input)

execSolver :: Board -> IO ()
execSolver = mapM_ printBoard . solver

