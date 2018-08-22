module Main where

import Data.Char ( isDigit, digitToInt)
import Text.CSV

import Sudoku

main :: IO ()
main = do
    csv <- parseCSVFromFile "./sudoku.csv"
    case csv of
        Left _ -> putStrLn "illegal format"
        Right b -> do
            let b' = concat . concat $ b
            if validate b'
                then putStrLn "illegal format"
                else do
                    let target = toBoard . map digitToInt $ b'
                    putStrLn "target:"
                    printBoard target
                    putStrLn "solutions:"
                    mapM_ printBoard $ solver target

validate :: String -> Bool
validate str
    | not $ all isDigit str = True
    | length str /= 81      = True
    | otherwise             = False

