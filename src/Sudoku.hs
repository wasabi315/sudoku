{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Sudoku
  ( Sudoku,
    parse,
    solve,
  )
where

import Control.Monad
import Data.Char
import Data.Foldable.Ext
import Data.Function
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word
import Math.ExactCover qualified as EC

-- Solving Sudoku is an exact cover problem
newtype Sudoku = Sudoku (Map Cell (Set Constraint))

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
          RowCol -> ('R', 'C')
          RowNum -> ('R', '#')
          ColNum -> ('C', '#')
          BoxNum -> ('B', '#')

constraints :: Map Cell (Set Constraint)
constraints =
  Map.fromDistinctAscList do
    row <- [0 .. 8]
    col <- [0 .. 8]
    num <- [0 .. 8]
    let box = 3 * (row `div` 3) + (col `div` 3)
        !cell = Cell row col num
        !constraintsOfCell =
          Set.fromDistinctAscList
            [ Constraint RowCol row col,
              Constraint RowNum row num,
              Constraint ColNum col num,
              Constraint BoxNum box num
            ]
    pure (cell, constraintsOfCell)

-- `mempty` for problem with no cell filled
newtype ParseHelper = ParseHelper (Endo [Cell])
  deriving newtype (Semigroup, Monoid)

-- place num on (row, col)
-- do not place on the same position
-- must place in accending order of the position
place :: Word8 -> Word8 -> Word8 -> ParseHelper
place row col num =
  ParseHelper . Endo $
    ( [ cell
        | num' <- [0 .. 8],
          num /= num',
          let !cell = Cell row col num'
      ]
        ++
    )

toSudoku :: ParseHelper -> Sudoku
toSudoku (ParseHelper builder) =
  Sudoku $
    Map.withoutKeys
      constraints
      (Set.fromDistinctAscList $ appEndo builder [])

parse :: String -> Maybe Sudoku
parse str =
  do
    guard $ length str == 81
    builder <-
      str
        & zip allPos
        & foldMapM \case
          ((row, col), char)
            | not (isAllowedChar char) -> Nothing
            | char == '.' -> Just mempty
            | otherwise ->
              Just (place row col $! fromIntegral (digitToInt char - 1))
    pure $! toSudoku builder
  where
    allPos = (,) <$> [0 .. 8] <*> [0 .. 8]

    -- ".123456789" ('.' for empty cell)
    isAllowedChar c = c == '.' || (isDigit c && c > '0')

solve :: Sudoku -> Maybe String
solve (Sudoku cstr) =
  cstr
    & EC.solve
    & fmap (map (intToDigit . fromIntegral . num) . Set.toAscList)
