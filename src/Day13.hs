{-# LANGUAGE ViewPatterns #-}

module Day13 (part1, part2) where

import Data.Ord (comparing)
import Data.List (minimumBy)
import Data.List.Split (splitOn)
import Paths_advent_of_code

part1 :: IO Int
part1 = do
  (timestamp, map read . filter (/= "x") -> buses) <- input

  let (bus, busTime) =
        minimumBy (comparing snd)
          . map (\x -> (x, (timestamp `div` x + 1) * x))
          $ buses

  return $ bus * (busTime - timestamp)



part2 :: IO Int
part2 = do
  (_, buses) <- input

  let ((_, firstBus) : offsets) =
        map (read <$>) . filter ((/= "x") . snd) . zip [0 ..] $ buses

  return $ solve2 firstBus 0 offsets


-- All buses are primes. Therefore we can increase stepSize by (* bus),
-- because the lowest common multiple of two primes is their product.
solve2 :: Int -> Int -> [(Int, Int)] -> Int
solve2 _ timestamp [] = timestamp
solve2 stepSize timestamp ((offset, bus):rest)
  | (timestamp + offset) `rem` bus == 0 = solve2 (stepSize * bus) timestamp rest
  | otherwise = solve2 stepSize (timestamp + stepSize) ((offset, bus):rest)


input :: IO (Int, [String])
input = do
  [read -> timestamp, splitOn "," -> buses] <-
    lines <$> (readFile =<< getDataFileName "inputs/day13.txt")

  return (timestamp, buses)

