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
part2 = return 13



input :: IO (Int, [String])
input = do
  [read -> timestamp, splitOn "," -> buses] <-
    lines <$> (readFile =<< getDataFileName "inputs/day13.txt")

  return (timestamp, buses)

