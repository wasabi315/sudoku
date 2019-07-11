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
import           Control.Monad
import           Data.Foldable
import           Data.Function
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
    | IS.null c = Nothing
    | otherwise = pure c
  where
    c = minimumBy (comparing IS.size) m


delete :: Matrix -> Int -> Matrix
delete m r = IM.map (`IS.difference` s) f
  where
    (t, f) = IM.partition (IS.member r) m
    s      = IS.unions t


algX :: Matrix -> [IS.IntSet]
algX (IM.null -> True) = [IS.empty]
algX m = case minSizeCol m of
    Nothing -> []
    Just rs -> do
        r <- IS.toAscList rs
        IS.insert r <$> algX (delete m r)

-------------------------------------------------------------------------------

toMatrix :: [[Int]] -> Matrix
toMatrix = foldr phi (const IM.empty) [(0 :: Int) ..]
  where
    phi _ _ []       = IM.empty
    phi r k (cs:css) = foldr psi (k css) cs
      where
        psi = IM.alter (Just . maybe (IS.singleton r) (IS.insert r))

