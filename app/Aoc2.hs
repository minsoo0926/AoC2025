module Aoc2 where

import Data.List.Split (splitOn)

checkInvalidId :: Int -> Bool
checkInvalidId a =
  case show a of
    [] -> False
    s -> uncurry (==) (splitAt (length s `div` 2) s)

stringToIds :: String -> [Int]
stringToIds s =
  case break (== '-') s of
    (a, '-' : b) -> [(read a) .. (read b)]
    _ -> error "invalid input"

main :: IO ()
main = do
  fileContent <- readFile "input2.txt"
  let ids = concatMap stringToIds $ splitOn "," $ head $ lines fileContent
      answer = sum $ filter checkInvalidId ids
  print answer
