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

import           Control.Monad
import           Data.Foldable
import           Data.Function
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

-------------------------------------------------------------------------------
-- TYPES

type Column = IS.IntSet
type Matrix = IM.IntMap Column

-------------------------------------------------------------------------------
-- ALGORITHM

minSizeCol :: Matrix -> Maybe Column
minSizeCol = IM.foldr f Nothing
  where
    f :: Column -> Maybe Column -> Maybe Column
    f c Nothing = pure c
    f c1 (Just c2)
        | IS.size c1 == 0         = Nothing
        | IS.size c1 < IS.size c2 = Just c1
        | otherwise               = Just c2


delete :: Int -> Matrix -> Matrix
delete r m =
    let (t, f) = IM.partition (IS.member r) m
        !s     = fold t
    in  IM.map (`IS.difference` s) f


algX :: Matrix -> [IS.IntSet]
algX m
    | IM.null m = [IS.empty]
    | otherwise = case mc of
        Nothing -> []
        Just is -> do
            r <- IS.toAscList is
            IS.insert r <$> algX (delete r m)
  where
    mc = minSizeCol m

