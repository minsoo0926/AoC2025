module Aoc8 where

import Control.Monad.State
import Data.Function (on)
import Data.List (findIndex, sortBy)
import Data.List.Split (splitOn)
import qualified Data.Set as Set

example :: String
example = "162,817,812\n57,618,57\n906,360,560\n592,479,940\n352,342,300\n466,668,158\n542,29,236\n431,825,988\n739,650,466\n52,470,668\n216,146,977\n819,987,18\n117,168,530\n805,96,715\n346,949,466\n970,615,88\n941,993,340\n862,61,35\n984,92,344\n425,690,689\n"

type Node = (Int, Int, Int)

type Edge = (Node, Node)

type Graph = ([Node], [Edge])

type Component = Set.Set Node

parseNode :: String -> Node
parseNode s =
  case parsedList of
    [x, y, z] -> (x, y, z)
    _ -> error "Invalid input"
  where
    parsedList = map read $ splitOn "," s

euclidianSquared :: Node -> Node -> Int
euclidianSquared (x1, y1, z1) (x2, y2, z2) = sum $ map (\x -> x * x) [dx, dy, dz]
  where
    dx = x1 - x2
    dy = y1 - y2
    dz = z1 - z2

findSmallestNNeighbors :: Int -> [Node] -> [Edge]
findSmallestNNeighbors n nodes = take n edges
  where
    edges = sortBy (compare `on` uncurry euclidianSquared) $ [(nodes !! i, nodes !! j) | i <- [0 .. length nodes - 1], j <- [i + 1 .. length nodes - 1]]

prodThreeLargestCC :: [Node] -> [Edge] -> Int
prodThreeLargestCC nodes edges = result
  where
    singletones = map Set.singleton nodes
    components = execState (mapM_ step edges) singletones
    result = product $ take 3 $ sortBy (flip compare) $ map Set.size components

-- TODO: implement disjoint set
step :: Edge -> State [Component] ()
step (v1, v2) = do
  components <- get
  let c1 = findIndex (Set.member v1) components
      c2 = findIndex (Set.member v2) components
  case (c1, c2) of
    (Just idx1, Just idx2) ->
      if idx1 == idx2
        then pure ()
        else
          let newComponent = Set.union (components !! idx1) (components !! idx2)
              newComponents = take (min idx1 idx2) components ++ drop (max idx1 idx2 + 1) components ++ (drop (min idx1 idx2 + 1) . take (max idx1 idx2)) components ++ [newComponent]
           in put newComponents
    _ -> error "Invalid edge: one of the nodes is not in any component"

solve :: String -> Int
solve s = result
  where
    nodes = map parseNode $ lines s
    result = prodThreeLargestCC nodes $ findSmallestNNeighbors 1000 nodes

main :: IO ()
main = do
  content <- readFile "input/input8.txt"
  print $ solve content
