{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Sudoku where

import Control.Monad
import Data.Char
import Data.Foldable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word
import Math.ExactCover qualified as EC

data Cell = Cell
  { row :: {-# UNPACK #-} Word8,
    col :: {-# UNPACK #-} Word8,
    num :: {-# UNPACK #-} Word8
  }
  deriving (Eq, Ord)

instance Show Cell where
  showsPrec _ Cell {..} =
    showChar 'R'
      . shows row
      . showChar 'C'
      . shows col
      . showChar '#'
      . shows num

data ConstraintKind
  = RowCol
  | RowNum
  | ColNum
  | BoxNum
  deriving (Eq, Ord)

data Constraint
  = Constraint ConstraintKind {-# UNPACK #-} Word8 {-# UNPACK #-} Word8
  deriving (Eq, Ord)

instance Show Constraint where
  showsPrec _ (Constraint kind n m) =
    showChar l1
      . shows n
      . showChar l2
      . shows m
    where
      (l1, l2) =
        case kind of
          RowCol ->
            ('R', 'C')
          RowNum ->
            ('R', '#')
          ColNum ->
            ('C', '#')
          BoxNum ->
            ('B', '#')

sudokuConstraints :: Map Cell (Set Constraint)
sudokuConstraints =
  Map.fromDistinctAscList do
    row <- [0 .. 8]
    col <- [0 .. 8]
    num <- [0 .. 8]
    let !box = 3 * (row `div` 3) + (col `div` 3)
        !cell = Cell row col num
        !constraints =
          Set.fromDistinctAscList
            [ Constraint RowCol row col,
              Constraint RowNum row num,
              Constraint ColNum col num,
              Constraint BoxNum box num
            ]
    pure (cell, constraints)

readSudoku :: String -> Maybe (Map Cell (Set Constraint))
readSudoku s =
  do
    guard $ length s == 81
    digits <- flip filterM (zip allPos s) \(_, c) ->
      if
          | not (isAllowedChar c) -> Nothing
          | c == '.' -> Just False
          | otherwise -> Just True

    let isExcluded =
          getAny . fold do
            ((r, c), d) <- digits
            let !n = fromIntegral (digitToInt d) - 1
            pure $ Any . \Cell {..} -> row == r && col == c && num /= n

    pure $ Map.filterWithKey (const . not . isExcluded) sudokuConstraints
  where
    allPos = (,) <$> [0 .. 8] <*> [0 .. 8]

    -- ".123456789" ('.' for empty cell)
    isAllowedChar c = c == '.' || (isDigit c && c > '0')

solveSudoku :: Map Cell (Set Constraint) -> Maybe String
solveSudoku c =
  map (intToDigit . fromIntegral . num) . Set.toAscList <$> EC.solve c
