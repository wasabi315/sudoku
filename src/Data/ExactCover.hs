{-# LANGUAGE BangPatterns #-}

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
import qualified Data.IntMap.Strict  as IM
import qualified Data.IntSet         as IS
import           Data.List
import           Data.Ord

import           Data.Foldable.Extra

-------------------------------------------------------------------------------
-- TYPES

type Column = IS.IntSet
type Matrix = IM.IntMap Column

-------------------------------------------------------------------------------
-- ALGORITHM

minSizeCol :: Matrix -> Column
minSizeCol = minimumBy (comparing IS.size)


shrink :: Matrix -> Int -> Matrix
shrink m r =
    let
        (t, f) = IM.partition (IS.member r) m
        s      = IS.unions t
    in
        IM.map (`IS.difference` s) f


algX :: Matrix -> [IS.IntSet]
algX m = case (IM.null m, minSizeCol m) of
    (True, _) -> []
    (_, !rs) -> do
        r <- IS.toAscList rs
        IS.insert r <$> algX (shrink m r)

-------------------------------------------------------------------------------

toMatrix :: [[Int]] -> Matrix
toMatrix = ifoldr phi IM.empty
  where
    phi r = flip . foldr . IM.alter $ \m ->
        IS.insert r <$> m <|> pure (IS.singleton r)
