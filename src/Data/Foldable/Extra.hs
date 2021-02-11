-------------------------------------------------------------------------------
-- |
-- Module      : Data.Foldable.Extra
-- Description : Foldable utils
-- Copyright   : (c) 2019 Satoshi Takimoto
-- Licence     : MIT
--
-- Utility function for Foldable
--
-------------------------------------------------------------------------------

module Data.Foldable.Extra where


ifoldr
    :: Foldable t
    => (Int -> a -> b -> b)
    -> b
    -> t a
    -> b
ifoldr f b ta = foldr (\a k i -> f i a (k $! i + 1)) (const b) ta 0
{-# INLINE ifoldr #-}
