{-# LANGUAGE ViewPatterns #-}

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

import           Control.Applicative
import           Data.Foldable
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import           Data.Ord

import           Data.Foldable.Extra

-------------------------------------------------------------------------------
-- TYPES

type Column = IS.IntSet
type Matrix = IM.IntMap Column

-------------------------------------------------------------------------------
-- ALGORITHM

minSizeCol :: Matrix -> Maybe Column
minSizeCol (IM.null -> True) = Nothing
minSizeCol m = Just $ minimumBy (comparing IS.size) m


shrink :: Matrix -> Int -> Matrix
shrink m r = IM.map (`IS.difference` s) f
  where
    (t, f) = IM.partition (IS.member r) m
    s      = IS.unions t


algX :: Matrix -> [IS.IntSet]
algX (IM.null -> True) = [IS.empty]
algX m = case minSizeCol m of
    Nothing -> []
    Just rs -> do
        r <- IS.toAscList rs
        IS.insert r <$> algX (shrink m r)

-------------------------------------------------------------------------------

toMatrix :: [[Int]] -> Matrix
toMatrix = ifoldr phi IM.empty
  where
    phi r = flip . foldr . IM.alter $ \m ->
        IS.insert r <$> m <|> pure (IS.singleton r)

