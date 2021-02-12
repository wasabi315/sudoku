module Math.ExactCover (solve) where

import Control.Monad.ST
import Data.Map.Strict (Map)
import Data.Set (Set)

solve :: Map (Set a) k -> ST s (Maybe (Set k))
solve _ =
    pure Nothing
