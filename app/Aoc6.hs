module Aoc6 where

import Data.List (transpose)

example :: String
example = "123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n*   +   *   +  "

parseIntList :: String -> [Int]
parseIntList = map read . words

foldOper :: Char -> [Int] -> Int
foldOper o l =
  case o of
    '+' -> sum l
    '*' -> product l
    _ -> error "arithmetic operator expected"

main :: IO ()
main = do
  content <- readFile "input6.txt"
  let calcList = transpose $ map parseIntList $ take (length l - 1) l
      operList = filter (/= ' ') $ l !! (length l - 1)
      l = lines content
  print $ sum $ zipWith foldOper operList calcList
