module Main where

import           Data.List
import           Data.Maybe
import           System.IO

parseLine :: String -> Maybe (Int, Int)
parseLine str =
    case words str of
      [a, b] -> Just (read a, read b)
      _      -> Nothing

linesToTwoLists :: [String] -> ([Int], [Int])
linesToTwoLists str = (sort l1, sort l2)
  where
    (l1, l2) = unzip . mapMaybe parseLine $ str

similarityScore :: [Int] -> Int -> Int
similarityScore xs n = n * (length . filter (== n) $ xs)

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    lines' <- lines <$> hGetContents handle
    let (l1, l2) = linesToTwoLists lines'
    -- Part 1
    print $ sum $ map abs $ zipWith (-) l1 l2
    -- Part 2
    print $ sum $ map (similarityScore l1) l2
