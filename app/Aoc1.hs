module Aoc1 where

rotate :: Int -> Char -> Int -> Int
rotate o c r = case c of
  'L' -> (o - r) `mod` 100
  'R' -> (o + r) `mod` 100
  _ -> error "invalid direction"

parse :: String -> (Char, Int)
parse (c : r) = (c, read r)
parse [] = error "empty string"

rotateAll :: Int -> [(Char, Int)] -> [Int]
rotateAll b commands = tail $ scanl step start commands
  where
    start = b
    step o (c, r) = rotate o c r

main :: IO ()
main = do
  content <- readFile "input1.txt"
  let commands = map parse $ lines content
      positions = rotateAll 50 commands
      answer = length $ filter (== 0) positions
  print answer
