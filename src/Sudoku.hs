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

import           Data.Char             ( digitToInt, intToDigit )
import           Data.IntMap.Strict    ( IntMap, (!) )
import qualified Data.IntMap.Strict    as IM
import           Data.List             ( sort, delete )
import           Data.Monoid           ( All(..), Any(..) )

-- Types ----------------------------------------------------------------------


data Pos = Pos
    { row :: Int
    , col :: Int
    , sqr :: Int
    } deriving (Eq, Show, Ord)


type Board = IntMap [Pos]


toBoard :: String -> Maybe Board
toBoard s
    | length s == 81 = Just . foldr f empty . zip allPos $ s
    | otherwise      = Nothing
  where
    empty = IM.fromList [ (n, []) | n <- [0..9] ]
    toSqr r c = 3 * div r 3 + div c 3
    allPos = [ Pos r c s | r <- [0..8], c <- [0..8], let s = toSqr r c ]
    f (p, n) = IM.adjust (p:) (readVal n)
    readVal '.' = 0
    readVal n   = digitToInt n


showBoard :: Board -> String
showBoard = map intToDigit . toList
  where
    toList            = map snd . sort . concatMap f . IM.toList
    f (i, xs)         = map (,i) xs

printBoard :: Board -> IO ()
printBoard = putStrLn . showBoard


-- Solver ---------------------------------------------------------------------

(#!) :: Pos -> Pos -> Bool
Pos r1 c1 s1 #! Pos r2 c2 s2
    =  r1 /= r2
    && c1 /= c2
    && s1 /= s2


assign :: Int -> Pos -> Board -> Board
assign n p = IM.adjust (p:) n . IM.adjust (delete p) 0


notMemberOn :: (Pos -> Int) -> Pos -> [Pos] -> Bool
notMemberOn f p ps = f p `notElem` map f ps


sieve :: Pos -> [Pos] -> Bool
sieve p = getAll . foldMap (All . (#! p))


independent :: Pos -> [Pos] -> Bool
independent p ps = getAny $ foldMap (\f -> Any $ notMemberOn f p ps) [row, col, sqr]


independents :: [Pos] -> [Pos]
independents ps = filter (\p -> independent p (delete p ps)) ps


determineBy :: Int -> Board -> Board
determineBy n b =
    foldr (assign n) b $ independents $ filter (`sieve` (b ! n)) (b ! 0)


determine :: Board -> Board
determine b = foldr determineBy b [1..9]


determineAll :: Board -> Board
determineAll b =
    let b' = determine b
    in  if b == b' then b else determineAll b'


candidatesAt :: Pos -> Board -> [Int]
candidatesAt p b = [ n | n <- [1..9], sieve p (b ! n) ]


candidates :: Board -> [[Int]]
candidates b = map (`candidatesAt` b) (b ! 0)


minIndex :: [Int] -> Int
minIndex = snd . minimum . (`zip` [0..])


assumptions :: Board -> [Board]
assumptions b =
    let css  = candidates b
        i    = minIndex $ map length css
        cand = css !! i
        co   = (b ! 0) !! i
    in [ assign n co b | n <- cand ]


solver :: Board -> [Board]
solver b
    | wrong b'      = []
    | null (b' ! 0) = [b']
    | otherwise     = concatMap solver $ assumptions b'
    where
        b' = determineAll b


wrong :: Board -> Bool
wrong b = getAny $ foldMap (Any . dup) (IM.delete 0 b)


dup :: [Pos] -> Bool
dup ps = getAny $ foldMap (Any . not . (\p -> p `sieve` delete p ps)) ps

