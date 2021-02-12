{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Sudoku where

import Control.Monad.ST
import Data.Char
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word

import Math.ExactCover

data Cell = Cell
    { row :: {-# UNPACK #-} Word8
    , col :: {-# UNPACK #-} Word8
    , num :: {-# UNPACK #-} Word8
    }
    deriving (Eq, Ord)

instance Show Cell where
    showsPrec _ Cell{..} =
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

readSudoku :: String -> Maybe (Map (Set Constraint) Cell)
readSudoku _ =
    Nothing

solveSudoku :: Map (Set Constraint) Cell -> Maybe String
solveSudoku c =
    map (chr . fromIntegral . num) . Set.toAscList <$> runST (solve c)
