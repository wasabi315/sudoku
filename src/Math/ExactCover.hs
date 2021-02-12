module Math.ExactCover (solve) where

import Control.Monad.ST
import Data.Map.Strict (Map)
import Data.Set (Set)

solve :: Map k (Set a) -> ST s (Maybe (Set k))
solve _ =
    pure Nothing
