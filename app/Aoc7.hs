module Aoc7 where

-- import Control.Monad (foldM)
-- import Control.Monad.State
--
example :: String
example = ".......S.......\n...............\n.......^.......\n...............\n......^.^......\n...............\n.....^.^.^.....\n...............\n....^.^...^....\n...............\n...^.^...^.^...\n...............\n..^...^.....^..\n...............\n.^.^.^.^.^...^.\n..............."

type BeamState = (Int, [Bool])

parseStart :: String -> [Bool]
parseStart = map (== 'S')

parseOthers :: String -> [Bool]
parseOthers = map (== '^')

splitMatched :: [Bool] -> [Bool] -> [Bool]
splitMatched prev next = zipWith (||) (False : matched) (tail matched ++ [False])
  where
    matched = zipWith (&&) prev next

-- step :: [Bool] -> [Bool] -> BeamState
-- step prev next = do
--   count <- get
--   let nextBeam = zipWith (\x y -> not x && y) next $ zipWith (||) (splitMatched prev next) prev
--       nextCount = count + length $ filter id $ zipWith (&&) prev next
--   put nextCount
--   return nextBeam
--
step :: BeamState -> [Bool] -> BeamState
step (count, prev) next = (nextCount, nextBeam)
  where
    nextBeam = zipWith (\x y -> not x && y) next $ zipWith (||) (splitMatched prev next) prev
    nextCount = count + length (filter id $ zipWith (&&) prev next)

solve :: String -> Int
solve s = fst result
  where
    l = lines s
    start = parseStart $ head l
    others = map parseOthers $ tail l
    result = foldl step (0, start) others

main :: IO ()
main = do
  content <- readFile "input7.txt"
  print $ solve content
