{-# LANGUAGE BangPatterns #-}

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
import qualified Data.IntSet         as IS

import           Data.ExactCover
import           Data.Foldable.Extra

-------------------------------------------------------------------------------
-- CONSTANT

sudokuConstraint :: Matrix
sudokuConstraint = toMatrix $ map cstr [0 .. 728]
  where
    cstr :: Int -> [Int]
    cstr x =
        let
            (i, n) = x `divMod` 9
            (r, c) = i `divMod` 9
            b = 3 * (r `div` 3) + (c `div` 3)
            !cstr1 = i
            !cstr2 = 9 * r + n + 81
            !cstr3 = 9 * c + n + 162
            !cstr4 = 9 * b + n + 243
        in
            [cstr1, cstr2, cstr3, cstr4]

-------------------------------------------------------------------------------
-- PROCESSING

readSudoku :: String -> Maybe IS.IntSet
readSudoku str
    | length str == 81 = Just (ifoldr phi IS.empty str)
    | otherwise        = Nothing
    where
        phi i c s =
            if isDigit c && c > '0'
                then IS.insert (9*i + digitToInt c - 1) s
                else s


showSudoku :: IS.IntSet -> String
showSudoku = map (\i -> intToDigit (i `mod` 9 + 1)) . IS.toAscList

-------------------------------------------------------------------------------
-- MAIN

solveSudoku :: String -> [String]
solveSudoku = maybe [] solve . readSudoku
  where
    solve is
        = map (showSudoku . IS.union is)
        . algX
        $ IS.foldl' shrink sudokuConstraint is
