module Aoc10 where

import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)

example :: String
example = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"

parseLine :: String -> ([Bool], [[Bool]])
parseLine s = (target, buttons)
  where
    ws = words s
    target = (\x -> take (length x - 1) x) $ map (== '#') $ drop 1 $ head ws
    buttons = map (\x -> (\y -> [i `elem` y | i <- [0 .. length target - 1]]) $ map read $ splitOn "," $ drop 1 $ take (length x - 1) x) $ (\x -> take (length x - 1) x) $ tail ws

powerSet :: [[Bool]] -> [[[Bool]]]
powerSet [] = [[]]
powerSet (x : xs) = map (x :) (powerSet xs) ++ powerSet xs

xor :: Bool -> Bool -> Bool
xor a b = a /= b

getButtonResult :: [Bool] -> [[Bool]] -> Maybe Int
getButtonResult target buttons = if all not $ foldl (zipWith xor) target buttons then Just (length buttons) else Nothing

solveLine :: ([Bool], [[Bool]]) -> Int
solveLine (_, []) = 0
solveLine (target, buttons) = minimumButton
  where
    buttonResults = map (getButtonResult target) $ powerSet buttons
    minimumButton = minimum $ catMaybes buttonResults

solve :: String -> Int
solve = sum . map (solveLine . parseLine) . lines

main :: IO ()
main = do
  content <- readFile "input/input10.txt"
  print $ solve content
