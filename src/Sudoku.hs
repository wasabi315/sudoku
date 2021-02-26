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
    Builder,
    runBuilder,
    place,
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

newtype DistPosCell = DistPosCell Cell

instance Eq DistPosCell where
  DistPosCell (Cell r1 c1 _) == DistPosCell (Cell r2 c2 _) =
    r1 == r2 && c1 == c2

instance Ord DistPosCell where
  DistPosCell (Cell r1 c1 _) `compare` DistPosCell (Cell r2 c2 _) =
    r1 `compare` r2 <> c1 `compare` c2

-- `mempty` for problem with no cell filled
newtype Builder = Builder (Set DistPosCell)
  deriving newtype (Semigroup, Monoid)

-- place num on (row, col)
place :: Word8 -> Word8 -> Word8 -> Builder
place row col num =
  Builder . Set.singleton . DistPosCell $! Cell row col num

runBuilder :: Builder -> Sudoku
runBuilder (Builder builder) =
  Sudoku . Map.withoutKeys constraints . Set.fromDistinctAscList $ do
    DistPosCell (Cell row col num) <- Set.toAscList builder
    num' <- filter (/= num) [0 .. 8]
    pure $! Cell row col num'

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
    pure $! runBuilder builder
  where
    allPos = (,) <$> [0 .. 8] <*> [0 .. 8]

    -- ".123456789" ('.' for empty cell)
    isAllowedChar c = c == '.' || (isDigit c && c > '0')

solve :: Sudoku -> Maybe String
solve (Sudoku cstr) =
  cstr
    & EC.solve
    & fmap (map (intToDigit . fromIntegral . num) . Set.toAscList)
