{-# LANGUAGE ViewPatterns #-}

-------------------------------------------------------------------------------
-- |
-- Module      : Sudoku
-- Description : 9x9 sudoku solver
-- Copyright   : (c) 2019 Satoshi Takimoto
-- Licence     : MIT
--
-- 9x9 sudoku solver using Knuth's Algorithm X.
--
-------------------------------------------------------------------------------

module Sudoku where

import           Data.Char
import qualified Data.IntSet as IS

import           Data.ExactCover
import           Data.Foldable.Extra

-------------------------------------------------------------------------------
-- UTIL

force :: a -> a
force x = x `seq` x

-------------------------------------------------------------------------------
-- CONSTANT

sudokuConstraint :: Matrix
sudokuConstraint = toMatrix $ map cstr [0 .. 728]
  where
    cstr :: Int -> [Int]
    cstr x =
        [ force i
        , force $ 9 * r + n + 81
        , force $ 9 * c + n + 162
        , force $ 9 * b + n + 243
        ]
      where
        (i, n) = x `divMod` 9
        (r, c) = i `divMod` 9
        b = 3 * (r `div` 3) + (c `div` 3)

-------------------------------------------------------------------------------
-- PROCESSING

readSudoku :: String -> Maybe IS.IntSet
readSudoku str@(length -> 81) = Just . IS.fromAscList $ ifoldr phi [] str
  where
    phi i c xs = if isDigit c && c > '0'
        then (9 * i + digitToInt c - 1) : xs
        else xs
readSudoku _ = Nothing


showSudoku :: IS.IntSet -> String
showSudoku = map (\i -> intToDigit $! i `mod` 9 + 1) . IS.toAscList

-------------------------------------------------------------------------------
-- MAIN

solveSudoku :: String -> [String]
solveSudoku = maybe [] solve . readSudoku
  where
    solve is
        = map (showSudoku . IS.union is)
        . algX
        $ IS.foldl' shrink sudokuConstraint is

