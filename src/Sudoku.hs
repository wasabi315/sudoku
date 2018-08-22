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
data Coord = Coord
    { row :: Int
    , col :: Int
    , sqr :: Int
    } deriving (Eq, Show, Ord)

-- | Each key represents 1-9, blank (= 0) and
--   each value is a set of coordinates of the grid filled with the key value
type Board = IntMap (Set Coord)

-- | Generate sudoku Board from String data
-- TODO: refactor this
toBoard :: String -> Board
toBoard ns = foldr' f empty $ toBoard' 0 ns where
    empty
        = IM.singleton 0 (Set.fromList [ Coord r c s | r <- [0..8], c <- [0..8], let s = toSqr r c ])
        `IM.union` IM.fromList [ (n, Set.empty) | n <- [1..9] ]
    f (n, c) = fill n c
    toSqr r c = 3 * (r `div` 3) + (c `div` 3)
    toBoard' _ []      = []
    toBoard' i (n:ns)  =
        if n == '0'
        then toBoard' (i + 1) ns
        else
            let (r, c) = i `divMod` 9
                s      = toSqr r c
            in  (digitToInt n, Coord r c s) : toBoard' (i + 1) ns

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
--  TODO: refactor this
--
showBoard :: Board -> String
showBoard b =
    let rs1 = [ IM.toList $ IM.map (Set.toList . Set.map col . Set.filter ((r ==) . row)) b | r <- [0..8] ]
        rs2 = map (map (showCell . snd) . sort . concatMap f) rs1
        rs3 = map (intersperse ' ' . intersperse' '|') rs2
        rs4 = intersperse' rule rs3
    in  unlines rs4
    where
        showCell n
            | n == 0    = ' '
            | otherwise = intToDigit n
        f (i, xs) = [ (x, i) | x <- xs ]
        intersperse' x xs = [x] ++ intersperse x xs ++ [x]
        rule = "+---+---+---+---+---+---+---+---+---+"

printBoard :: Board -> IO ()
printBoard = putStrLn . showBoard


-- Solver ---------------------------------------------------------------------

-- | Fill the cell at 'c' with 'n'
fill :: Int -> Coord -> Board -> Board
{-# INLINE fill #-}
fill n c b = IM.adjust (Set.insert c) n (IM.adjust (Set.delete c) 0 b)

-- |
notMemberOn :: (Coord -> Int) -> Coord -> Set Coord -> Bool
{-# INLINE notMemberOn #-}
notMemberOn f c cs = f c `Set.notMember` Set.map f cs

-- |
sieve :: Coord -> Set Coord -> Bool
{-# INLINE sieve #-}
sieve c cs = foldr ((&&) . (\f -> notMemberOn f c cs)) True [row, col, sqr]

-- |
independent :: Coord -> Set Coord -> Bool
{-# INLINE independent #-}
independent c cs = foldr ((||) . (\f -> notMemberOn f c cs)) False [row, col, sqr]

-- |
independents :: Set Coord -> Set Coord
{-# INLINE independents #-}
independents cs = Set.filter (\c -> independent c (Set.delete c cs)) cs

-- |
determineBy :: Int -> Board -> Board
determineBy n b =
    Set.foldr' (fill n) b $ independents $ Set.filter (\c -> sieve c (b ! n)) (b ! 0)

-- |
determine :: Board -> Board
determine b = foldr determineBy b [1..9]

-- |
determineAll :: Board -> Board
determineAll b = loop (determine b) b where
    loop x y
        | x == y     = y
        | otherwise = loop (determine x) x

-- |
candidatesAt :: Coord -> Board -> [Int]
candidatesAt c b = [ n | n <- [1..9], sieve c (b ! n) ]

-- |
candidates :: Board -> [[Int]]
candidates b = map (`candidatesAt` b) $ Set.toAscList (b ! 0)

-- |
indexOfMin :: [Int] -> Int
indexOfMin []     = error "[indexOfMin: Empty List]"
indexOfMin (x:xs) = helper 0 x 1 xs where
    helper im _ _ []     = im
    helper im m i (x:xs)
        | m > x     = helper i x (i + 1) xs
        | otherwise = helper im m (i + 1) xs

-- |
assumptions :: Board -> [Board]
assumptions b =
    let css  = candidates b
        i    = indexOfMin $ map length css
        cand = css !! i
        co   = Set.elemAt i (b ! 0)
    in [ fill n co b | n <- cand ]

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
{-# INLINE wrong #-}
wrong b = IM.foldr ((||) . dup) False (IM.delete 0 b)

-- |
dup :: Set Coord -> Bool
{-# INLINE dup #-}
dup cs = not $ Set.foldr' ((&&) . (\c -> c `sieve` Set.delete c cs)) True cs


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

