module Aoc4 where

countAdjacent :: [[Bool]] -> [[Int]]
countAdjacent xss = [[countCell i j | j <- [0 .. col - 1]] | i <- [0 .. row - 1]]
  where
    row = length xss
    col = length (head xss)
    countCell i j =
      if xss !! i !! j
        then length $ filter id [xss !! k !! l | k <- [i - 1 .. i + 1], k >= 0 && k < row, l <- [j - 1 .. j + 1], l >= 0 && l < col, k /= i || l /= j]
        else -1

example :: String
example = "..@@.@@@@.\n@@@.@.@.@@\n@@@@@.@.@@\n@.@@@@..@.\n@@.@@@@.@@\n.@@@@@@@.@\n.@.@.@.@@@\n@.@@@.@@@@\n.@@@@@@@@.\n@.@.@@@.@."

main :: IO ()
main = do
  content <- readFile "input4.txt"
  let grid = map (map (== '@')) $ lines content
  print $ length $ concatMap (filter (< 4) . filter (>= 0)) $ countAdjacent grid

-- print $ countAdjacent grid
