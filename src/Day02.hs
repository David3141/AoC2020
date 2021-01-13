{-# LANGUAGE ViewPatterns #-}

module Day02 (part1, part2) where

import Helpers (readInts)
import Data.List.Split (splitOn, wordsBy)
import Paths_advent_of_code

part1 :: IO Int
part1 = length . filter matchesOldRule <$> input

part2 :: IO Int
part2 = length . filter matchesNewRule <$> input

input :: IO [(Int, Int, Char, String)]
input = map parse . lines <$> (readFile =<< getDataFileName "inputs/day02.txt")

parse :: String -> (Int, Int, Char, String)
parse line = (min, max, char, pw)
  where [read -> min, read -> max, [char], pw] = wordsBy (`elem` " -:") line

matchesOldRule :: (Int, Int, Char, String) -> Bool
matchesOldRule (min, max, char, pw) = min <= actualCount && actualCount <= max
  where actualCount = length . filter (== char) $ pw

matchesNewRule :: (Int, Int, Char, String) -> Bool
matchesNewRule (first, second, char, pw) = (== 1)
  . length
  . filter (== char)
  . map ((!!) pw . pred)
  $ [first, second]
