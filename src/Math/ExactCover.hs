{-# LANGUAGE BlockArguments #-}

module Math.ExactCover (solve) where

import Control.Monad.ST
import Data.Map.Strict (Map)
import Data.Set (Set)

solve :: Map k (Set a) -> Maybe (Set k)
solve _ = runST do
  pure Nothing
