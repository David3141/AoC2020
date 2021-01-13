module Day03 (part1, part2) where

import Data.List.Split (splitOn)
import Helpers (readInts)
import Paths_advent_of_code

part1 :: IO Int
part1 = countTreeEncounters (3, 1) <$> input

part2 :: IO Int
part2 = do
  lanes <- input
  return . product . map (`countTreeEncounters` lanes)
    $ [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

input :: IO [String]
input = map cycle . lines <$> (readFile =<< getDataFileName "inputs/day03.txt")

countTreeEncounters :: (Int, Int) -> [String] -> Int
countTreeEncounters (right, down) lanes =
  let numLanes = ceiling $ fromIntegral (length lanes) / fromIntegral down
      indices = take numLanes $ zip [0, right ..] [0, down ..]
  in length [1 | (x, y) <- indices, lanes !! y !! x == '#']
