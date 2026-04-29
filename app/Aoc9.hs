module Aoc9 where

example :: String
example = "7,1\n11,1\n11,7\n9,7\n9,5\n2,5\n2,3\n7,3"

type Coord = (Int, Int)

parseCoord :: String -> Coord
parseCoord s =
  case break (== ',') s of
    (a, ',' : b) -> (read a, read b)
    _ -> error "invalid input"

getArea :: (Coord, Coord) -> Int
getArea ((x1, y1), (x2, y2)) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

solve :: String -> Int
solve input = result
  where
    coords = map parseCoord $ lines input
    pairs = [(coords !! i, coords !! j) | i <- [0 .. length coords - 1], j <- [i + 1 .. length coords - 1]]
    result = maximum $ map getArea pairs

main :: IO ()
main = do
  input <- readFile "input/input9.txt"
  print $ solve input
