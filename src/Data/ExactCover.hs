-------------------------------------------------------------------------------
-- |
-- Module      : Data.ExactCover
-- Description : Knuth's Algorithm X implementation
-- Copyright   : (c) 2019 Satoshi Takimoto
-- Licence     : MIT
--
-- Knuth's Algorithm X implementation in Haskell.
--
-------------------------------------------------------------------------------

module Data.ExactCover where

import           Control.Monad
import           Data.Foldable
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import           Data.Ord

-------------------------------------------------------------------------------
-- TYPES

type Column = IS.IntSet
type Matrix = IM.IntMap Column

-------------------------------------------------------------------------------
-- ALGORITHM

minSizeCol :: Matrix -> Maybe Column
minSizeCol m
    | IS.size c == 0 = Nothing
    | otherwise      = pure c
  where
    c = minimumBy (comparing IS.size) m


delete :: Int -> Matrix -> Matrix
delete r m = IM.map (`IS.difference` s) f
  where
    (t, f) = IM.partition (IS.member r) m
    s      = fold t


algX :: Matrix -> [IS.IntSet]
algX m
    | IM.null m = [IS.empty]
    | otherwise = case minSizeCol m of
        Nothing -> []
        Just rs -> do
            r <- IS.toAscList rs
            IS.insert r <$> algX (delete r m)

