module Day10 (part1, part2) where

import Data.List (sort)

import Helpers (readInts)

part1 :: IO Int
part1 = do
  diffs <- differences . (0 :) . sort <$> input

  let j1 = length . filter (== 1) $ diffs
      j3 = (+ 1) . length . filter (== 3) $ diffs

  return $ j1 * j3

part2 :: IO Int
part2 = return 10

input :: IO [Int]
input = readInts "inputs/day10.txt"

differences :: [Int] -> [Int]
differences [x, y]         = [y - x]
differences (x : y : rest) = (y - x) : differences (y : rest)
