import           Data.List.Split
import           Test.HUnit

import           Sudoku

main :: IO ()
main = do
    ps <- lines <$> readFile "./test/sudoku.csv"
    let ts = map genTest ps
    runTestTT (TestList ts)
    pure ()


genTest :: String -> Test
genTest t = "test" ~: solveSudoku p ~?= [s]
  where
    [p, s] = wordsBy (== ',') t

