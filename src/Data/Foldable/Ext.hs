module Data.Foldable.Ext (foldMapM) where

import Control.Monad
import Data.Foldable

foldMapM :: (Monad m, Monoid b, Foldable t) => (a -> m b) -> t a -> m b
foldMapM f = foldlM (\acc a -> mappend acc <$!> f a) mempty
{-# INLINE foldMapM #-}
