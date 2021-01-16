module Day10 (part1, part2) where

import Data.List (sort)
import Data.List.Split (splitWhen)

import Helpers (readInts)

part1 :: IO Int
part1 = do
  diffs <- differences <$> input

  let j1 = length . filter (== 1) $ diffs
      j3 = length . filter (== 3) $ diffs

  return $ j1 * j3

part2 :: IO Int -- https://www.youtube.com/watch?v=LjrCckaHjB0
part2 = do
  nums <- input

  -- The mapping counts all possible connections up to the current element.
  -- By dropping everything that is further away than three (and the takeWhile
  -- up to current element) each time, this imitates looking back from each element
  -- (while in reality, it's starting at the beginning for each element and dropping
  -- everything that's not connectable).
  -- Example:
  -- Input:  [0, 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, 23]
  -- Counts: [1, 1, 1, 1, 2, 4,  4,  4,  8,  8,  8,  8,  8]
  let counts = 1 : map
        (\x ->
          sum                                  -- Sum counts to previous allowed connections
            $ map snd                          -- Only take the `count`
            $ takeWhile (\(y, _) -> y < x)     -- Take all up to current element
            $ dropWhile (\(y, _) -> x - y > 3) -- Drop elements being too far away
            $ zip nums counts                  -- Self-referring `counts`
        )
        (tail nums)

  return $ last counts

input :: IO [Int]
input = do
  ints <- sort <$> readInts "inputs/day10.txt"
  return $ [0] ++ ints ++ [last ints + 3]

differences :: [Int] -> [Int]
differences [x, y]         = [y - x]
differences (x : y : rest) = (y - x) : differences (y : rest)
