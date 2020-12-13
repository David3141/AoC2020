module Day04 (part1, part2) where

import Data.List (delete, groupBy)
import Data.List.Split (splitOn)
import Paths_advent_of_code

part1 :: IO Int
part1 = countBy hasAllRequiredTokens <$> readPassports

part2 :: IO Int
part2 = return 0

readPassports :: IO [[String]]
readPassports =
  map (concatMap words) . groupBy (\c1 c2 -> c2 /= "") . lines
    <$> (readFile =<< getDataFileName "inputs/day04.txt")

hasAllRequiredTokens :: [String] -> Bool
hasAllRequiredTokens = null . filterRequiredTokens requiredTokens
  where
    requiredTokens = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

filterRequiredTokens :: [String] -> [String] -> [String]
filterRequiredTokens remainingTokens [] = remainingTokens
filterRequiredTokens [] _ = []
filterRequiredTokens remainingTokens (pair : pairs) =
  filterRequiredTokens newRemainingTokens pairs
  where
    currentToken = head . splitOn ":" $ pair

    newRemainingTokens = delete currentToken remainingTokens

countBy :: (a -> Bool) -> [a] -> Int
countBy cond [] = 0
countBy cond (x : xs)
  | cond x = countBy cond xs + 1
  | otherwise = countBy cond xs
