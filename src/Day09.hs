module Day09 (part1, part2) where

import Helpers (readInts)

part1 :: IO Int
part1 = firstInvalidNum <$> input

part2 :: IO Int
part2 = return 9

input :: IO [Int]
input = readInts "inputs/day09.txt"

firstInvalidNum :: [Int] -> Int
firstInvalidNum (x:rest)
  | hasInvalidNum = candidate
  | otherwise = firstInvalidNum rest
  where
  (preamble, [candidate]) = splitAt 25 $ take 26 (x:rest)
  matchingPairs = filter (\(x, y) -> x + y == candidate) (pairs preamble)
  hasInvalidNum = null matchingPairs

pairs :: Eq a => [a] -> [(a, a)]
pairs list = [(x, y) | x <- list, y <- list, x /= y]
