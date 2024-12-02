module Main where

import           Data.Maybe
import           System.IO
import           Text.Read

parseLine :: String -> Maybe [Int]
parseLine = mapM readMaybe . words

isIncreasing :: [Int] -> Bool
isIncreasing (x:xs) = all (> x) xs && isIncreasing xs
isIncreasing []     = True

isDecreasing :: [Int] -> Bool
isDecreasing (x:xs) = all (< x) xs && isDecreasing xs
isDecreasing []     = True

checkSafety :: [Int] -> Bool
checkSafety l =
    (isIncreasing l && checkSafetyIncreasing l) ||
    (isDecreasing l && checkSafetyDecreasing l)
  where
    checkSafetyIncreasing (x:y:xs) = y - x <= 3 && checkSafetyIncreasing (y:xs)
    checkSafetyIncreasing _        = True
    checkSafetyDecreasing (x:y:xs) = x - y <= 3 && checkSafetyDecreasing (y:xs)
    checkSafetyDecreasing _        = True

removeOneAndCheckSafety :: [Int] -> Bool
removeOneAndCheckSafety l = any checkSafety (l : combinations)
  where
    combinations =
      map (\n -> take (n-1) l ++ drop n l) [1..length l]

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    lines' <- lines <$> hGetContents handle
    let reports = mapMaybe parseLine lines'
    -- Part 1
    let safeties = map checkSafety reports
    print $ length $ filter id safeties
    -- Part 2
    let safeties' = map removeOneAndCheckSafety reports
    print $ length $ filter id safeties'
