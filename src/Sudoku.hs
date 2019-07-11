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

import           Control.Arrow
import           Data.Char
import           Data.Foldable
import           Data.Function
import qualified Data.IntSet as IS
import           Data.List   ( unfoldr )
import           Data.Maybe

import           Data.ExactCover

-------------------------------------------------------------------------------
-- CONSTANT

sudokuConstraint :: Matrix
sudokuConstraint = toMatrix $ map cstr [0 .. 728]
  where
    cstr x =
        [ i
        , 9 * r + n + 81
        , 9 * c + n + 162
        , 9 * b + n + 243
        ]
      where
        (i, n) = x `divMod` 9
        (r, c) = i `divMod` 9
        b = 3 * (r `div` 3) + (c `div` 3)

-------------------------------------------------------------------------------
-- PROCESSING

readCell :: Char -> Maybe Int
readCell c =
    if isDigit c && c > '0'
        then Just $! digitToInt c - 1
        else Nothing


readSudoku :: String -> Maybe IS.IntSet
readSudoku str
    | length str /= 81 = Nothing
    | otherwise
        = zip [(0 :: Int) ..] str
        & mapMaybe (\(i, c) -> (+ (9 * i)) <$> readCell c)
        & IS.fromAscList
        & Just


showSudoku :: IS.IntSet -> String
showSudoku
    =   IS.toAscList
    >>> map (\i -> intToDigit $ i `mod` 9 + 1)

-------------------------------------------------------------------------------
-- MAIN

solveSudoku :: String -> [String]
solveSudoku = readSudoku >>> maybe [] solve
  where
    solve is
        = IS.foldl' delete sudokuConstraint is
        & algX
        & map (showSudoku . IS.union is)

