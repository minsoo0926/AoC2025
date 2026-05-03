{-# LANGUAGE TupleSections #-}

module Aoc11 where

import Control.Monad.State
import Data.List (nub)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

example :: String
example = "aaa: you hhh\nyou: bbb ccc\nbbb: ddd eee\nccc: ddd eee fff\nddd: ggg\neee: out\nfff: out\nggg: out\nhhh: ccc fff iii\niii: out"

type Node = String

type Edge = (String, String)

type Status = (M.Map Node Int, [Edge])

parseLine :: String -> [Edge]
parseLine line = case words line of
  [] -> error "Empty line"
  (node : rest) -> edges (take 3 node) rest
  where
    edges s = map (s,)

constructStatus :: String -> Status
constructStatus input = (countStatus, edges)
  where
    ls = lines input
    initialCount node = if node == "you" then (node, 1) else (node, 0)
    edges = concatMap parseLine ls
    nodes = nub $ concatMap (\(s, t) -> [s, t]) edges
    countStatus = M.fromList $ map initialCount nodes

step :: State Status ()
step = do
  (countStatus, edges) <- get
  let counts = map getPathCount edges
      getPathCount (s, t) = (t, fromMaybe 0 $ M.lookup s countStatus)
      acc = M.fromListWith (+) counts
      newStatus = M.insert "you" 1 acc
  put (newStatus, edges)

solve :: String -> Int
solve input = fromMaybe 0 result
  where
    (countStatus, edges) = constructStatus input
    result = M.lookup "out" $ fst $ execState (mapM_ (const step) [() | _ <- [1 .. M.size countStatus]]) (countStatus, edges)

main :: IO ()
main = do
  content <- readFile "input/input11.txt"
  print $ solve content
