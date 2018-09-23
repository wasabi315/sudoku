{-# LANGUAGE TupleSections #-}
{-
-- |
-- Module     : Sudoku
-- Maintainer : wasabi315
-- License    : MIT
--
-- 9x9 sudoku solver
--
-}
module Sudoku where

import           Data.Char             ( digitToInt, intToDigit, isDigit )
import           Data.Foldable         ( foldr' )
import           Data.IntMap.Strict    ( IntMap, (!) )
import qualified Data.IntMap.Strict    as IM
import           Data.List             ( (\\), sort, intersperse )
import           Data.List.Split       ( chunksOf )
import           Data.Monoid
import           Data.Set              ( Set )
import qualified Data.Set              as Set
import           Text.Parsec
import           Text.Parsec.String

-- Types ----------------------------------------------------------------------

-- | Coordinate of sudoku board
-- 'sqr' is the index that points 3x3 square shown as below
--
--    +---+---+---+
--    | 0 | 1 | 2 |
--    +---+---+---+
--    | 3 | 4 | 5 |
--    +---+---+---+
--    | 6 | 7 | 8 |
--    +---+---+---+
--
data Pos = Pos
    { row :: Int
    , col :: Int
    , sqr :: Int
    } deriving (Eq, Show, Ord)

-- | Each key represents 1-9, blank (= 0) and
--   each value is a set of coordinates of the grid filled with the key value
type Board = IntMap (Set Pos)

-- | Generate sudoku Board from String data
toBoard :: String -> Board
toBoard = foldr' f empty . zip allPos
  where
    empty = IM.fromList [ (n, Set.empty) | n <- [0..9] ]
    toSqr r c = 3 * div r 3 + div c 3
    allPos = [ Pos r c s | r <- [0..8], c <- [0..8], let s = toSqr r c ]
    f (p, n) = IM.adjust (Set.insert p) (digitToInt n)

-- | Generate ascii art of sudoku board form Board data
-- e.g.)
--
--         +---+---+---+---+---+---+---+---+---+
--         |   |   |   |   |   |   |   |   |   |
--         +---+---+---+---+---+---+---+---+---+
--         |   |   |   |   |   |   |   | 2 | 7 |
--         +---+---+---+---+---+---+---+---+---+
--         | 4 |   |   | 6 |   | 8 |   |   |   |
--         +---+---+---+---+---+---+---+---+---+
--         |   | 7 | 1 |   |   |   | 3 |   |   |
--         +---+---+---+---+---+---+---+---+---+
--         | 2 | 3 | 8 | 5 |   | 6 | 4 | 1 | 9 |
--         +---+---+---+---+---+---+---+---+---+
--         | 9 | 6 | 4 | 1 |   |   | 7 | 5 |   |
--         +---+---+---+---+---+---+---+---+---+
--         | 3 | 9 | 5 |   | 2 | 7 | 8 |   |   |
--         +---+---+---+---+---+---+---+---+---+
--         | 1 | 8 | 2 |   | 6 |   | 9 | 7 | 4 |
--         +---+---+---+---+---+---+---+---+---+
--         |   | 4 | 6 | 8 | 1 | 9 | 2 |   | 5 |
--         +---+---+---+---+---+---+---+---+---+
--
--
showBoard :: Board -> String
showBoard = format . toList
  where
    format            = unlines . intersperse' rule . map fmtRow . chunksOf 9
    fmtRow            = intersperse ' ' . intersperse' '|' . map showCell
    showCell 0        = ' '
    showCell n        = intToDigit n
    rule              = "+---+---+---+---+---+---+---+---+---+"
    intersperse' x xs = [x] ++ intersperse x xs ++ [x]
    toList            = map snd . sort . concatMap f . IM.toList
    f (i, xs)         = map (,i) (Set.toList xs)

printBoard :: Board -> IO ()
printBoard = putStrLn . showBoard


-- Solver ---------------------------------------------------------------------

-- |
assign :: Int -> Pos -> Board -> Board
assign n c = IM.adjust (Set.insert c) n . IM.adjust (Set.delete c) 0

-- |
notMemberOn :: (Pos -> Int) -> Pos -> Set Pos -> Bool
notMemberOn f c cs = f c `Set.notMember` Set.map f cs

-- |
sieve :: Pos -> Set Pos -> Bool
sieve c cs = getAll $ foldMap (\f -> All $ notMemberOn f c cs) [row, col, sqr]

-- |
independent :: Pos -> Set Pos -> Bool
independent c cs = getAny $ foldMap (\f -> Any $ notMemberOn f c cs) [row, col, sqr]

-- |
independents :: Set Pos -> Set Pos
independents cs = Set.filter (\c -> independent c (Set.delete c cs)) cs

-- |
determineBy :: Int -> Board -> Board
determineBy n b =
    Set.foldr' (assign n) b $ independents $ Set.filter (`sieve` (b ! n)) (b ! 0)

-- |
determine :: Board -> Board
determine b = foldr' determineBy b [1..9]

-- |
determineAll :: Board -> Board
determineAll b = loop (determine b) b where
    loop x y
        | x == y     = y
        | otherwise = loop (determine x) x

-- |
candidatesAt :: Pos -> Board -> [Int]
candidatesAt c b = [ n | n <- [1..9], sieve c (b ! n) ]

-- |
candidates :: Board -> [[Int]]
candidates b = map (`candidatesAt` b) $ Set.toAscList (b ! 0)

-- |
minIndex :: [Int] -> Int
minIndex = snd . minimum . (`zip` [0..])

-- |
assumptions :: Board -> [Board]
assumptions b =
    let css  = candidates b
        i    = minIndex $ map length css
        cand = css !! i
        co   = Set.elemAt i (b ! 0)
    in [ assign n co b | n <- cand ]

-- |
solver :: Board -> [Board]
solver b
    | wrong b'          = []
    | Set.null (b' ! 0) = [b']
    | otherwise         = concatMap solver $ assumptions b'
    where
        b' = determineAll b

-- |
wrong :: Board -> Bool
wrong b = getAny $ foldMap (Any . dup) (IM.delete 0 b)

-- |
dup :: Set Pos -> Bool
dup cs = not $ getAll $ foldMap (All . (\c -> c `sieve` Set.delete c cs)) cs


-- Parser ---------------------------------------------------------------------

sudoku :: Parser String
sudoku = do
    x <- count 81 cell
    eof
    return x

cell :: Parser Char
cell = skipMany (noneOf ['0'..'9']) *> digit <* skipMany (noneOf ['0'..'9'])

parseSudokuFromFile :: FilePath -> IO (Either ParseError String)
parseSudokuFromFile = parseFromFile sudoku

