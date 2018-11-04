{-# LANGUAGE TupleSections #-}
{-
--
-- Module     : Sudoku
-- Maintainer : wasabi315
-- License    : MIT
--
-- 9x9 sudoku solver
--
-}
module Sudoku where

import           Control.Arrow         ( (&&&) )
import           Control.Monad         ( foldM, guard )
import           Data.Char             ( digitToInt, intToDigit, isDigit )
import           Data.Function         ( on )
import           Data.IntMap.Strict    ( IntMap, (!) )
import qualified Data.IntMap.Strict    as IM
import           Data.List             ( sort, delete, minimumBy, partition )
import           Data.Monoid           ( All(..), Any(..) )
import           Data.Ord              ( comparing )

-- Types ----------------------------------------------------------------------

data Pos = Pos
    { row :: {-# UNPACK #-} !Int
    , col :: {-# UNPACK #-} !Int
    , sqr :: {-# UNPACK #-} !Int
    } deriving (Eq, Ord)

type Board = IntMap [Pos]

-- Utils ----------------------------------------------------------------------

allPos :: [Pos]
allPos = do
    r <- [0..8]
    c <- [0..8]
    let s = 3 * (r `div` 3) + (c `div` 3)
    return $! Pos r c s

readVal :: Char -> Maybe Int
readVal '.' = return $! 0
readVal c   = if isDigit c then return $! digitToInt c else Nothing

toBoard :: String -> Maybe Board
toBoard s
    | length s == 81 = foldM f empty . zip allPos $ s
    | otherwise      = Nothing
  where
    empty = IM.fromList $! map (, []) [0..9]
    f b (p, c) = (\n -> IM.adjust (p:) n b) <$> readVal c

showBoard :: Board -> String
showBoard = map snd . sort . concatMap f . IM.toList
  where
    f (i, xs) = map (, intToDigit i) xs

printBoard :: Board -> IO ()
printBoard = putStrLn . showBoard


-- Solver ---------------------------------------------------------------------

assign :: Int -> Pos -> Board -> Board
assign n p = IM.adjust (p:) n . IM.adjust (delete p) 0

sieve :: Pos -> [Pos] -> Bool
sieve (Pos r1 c1 s1) = getAll . foldMap (All . pred)
  where
    pred (Pos r2 c2 s2)
        =  r1 /= r2
        && c1 /= c2
        && s1 /= s2

notMemberOn :: (Pos -> Int) -> Pos -> [Pos] -> Bool
notMemberOn f p = not . any (((==) `on` f) p)

independent :: Pos -> [Pos] -> Bool
independent p ps
    =  notMemberOn row p ps'
    || notMemberOn col p ps'
    || notMemberOn sqr p ps'
  where
    ps' = delete p ps

independents :: [Pos] -> [Pos]
independents ps = filter (`independent` ps) ps

determineBy :: Int -> Board -> Board
determineBy n b =
    foldr (assign n) b $ independents $ filter pred (b ! 0)
  where
    pred p = sieve p (b ! n)

determine :: Board -> Board
determine b = foldr determineBy b [1..9]

determineAll :: Board -> Board
determineAll b = if b == b' then b else determineAll b'
  where
    b' = determine b

candidatesAt :: Pos -> Board -> [Int]
candidatesAt p b = [ n | n <- [1..9], pred n ]
  where
    pred n = sieve p (b ! n)

assumptions :: Board -> [Board]
assumptions b = map (\c -> assign c p b) cs
  where
    ps      = map (id &&& (`candidatesAt` b)) (b ! 0)
    (p, cs) = minimumBy (comparing (length . snd)) ps

solver :: Board -> [Board]
solver b
    | wrong b'    = []
    | isSolved b' = [b']
    | otherwise   = concatMap solver $ assumptions b'
  where
    b' = determineAll b

wrong :: Board -> Bool
wrong b = getAny $ foldMap (Any . dup) (IM.delete 0 b)

dup :: [Pos] -> Bool
dup ps = getAny $ foldMap (Any . pred) ps
  where
    pred p = not $ sieve p (delete p ps)

isSolved :: Board -> Bool
isSolved b = null (b ! 0)

