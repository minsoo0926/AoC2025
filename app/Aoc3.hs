module Aoc3 where

import Data.Char (digitToInt)

findLargestJoltage :: Int -> [Int] -> Int
findLargestJoltage s [] = s
findLargestJoltage s [x] =
  if s `mod` 10 < x then (s `div` 10) * 10 + x else s
findLargestJoltage s (x : y : xs)
  | s == 0 || x * 10 > s = findLargestJoltage (x * 10) (y : xs)
  | s `mod` 10 < x = findLargestJoltage ((s `div` 10) * 10 + x) (y : xs)
  | otherwise = findLargestJoltage s (y : xs)

main :: IO ()
main = do
  content <- readFile "input3.txt"

  -- print $ sum $ map (findLargestJoltage 0 . map digitToInt) $ lines content

  print $ map (findLargestJoltage 0 . map digitToInt) $ lines content
