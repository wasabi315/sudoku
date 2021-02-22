{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Sudoku where

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
    let !box = 3 * (row `div` 3) + (col `div` 3)
        !cell = Cell row col num
        !constraintsOfCell =
          Set.fromDistinctAscList
            [ Constraint RowCol row col,
              Constraint RowNum row num,
              Constraint ColNum col num,
              Constraint BoxNum box num
            ]
    pure (cell, constraintsOfCell)

parse :: String -> Maybe (Map Cell (Set Constraint))
parse str =
  do
    guard $ length str == 81
    excludedCells <-
      str
        & zip allPos
        & foldMapM \case
          ((row, col), char)
            | not (isAllowedChar char) -> Nothing
            | char == '.' -> Just mempty
            | otherwise ->
              Just . Endo $
                ( [ cell
                    | let !n = fromIntegral (digitToInt char - 1),
                      num <- [0 .. 8],
                      num /= n,
                      let !cell = Cell row col num
                  ]
                    ++
                )
        & fmap (Set.fromDistinctAscList . flip appEndo [])
    pure $! constraints `Map.withoutKeys` excludedCells
  where
    allPos = (,) <$> [0 .. 8] <*> [0 .. 8]

    -- ".123456789" ('.' for empty cell)
    isAllowedChar c = c == '.' || (isDigit c && c > '0')

solve :: Map Cell (Set Constraint) -> Maybe String
solve c =
  map (intToDigit . fromIntegral . num) . Set.toAscList <$> EC.solve c
