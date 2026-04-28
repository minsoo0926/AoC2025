module Aoc5 where

parseRange :: String -> (Int, Int)
parseRange s =
  case break (== '-') s of
    (a, '-' : b) -> (read a, read b)
    _ -> error "invalid input"

match :: Int -> (Int, Int) -> Bool
match i (a, b) = a <= i && i <= b

countFreshIds :: [Int] -> [(Int, Int)] -> Int
countFreshIds input target =
  sum [1 | i <- input, any (match i) target]

example :: String
example = "3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32"

main :: IO ()
main = do
  content <- readFile "input5.txt"
  let ranges = map parseRange $ takeWhile (/= "") $ lines content
      freshIds = map (read :: String -> Int) $ tail $ dropWhile (/= "") $ lines content
  print $ countFreshIds freshIds ranges
