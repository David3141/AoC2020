module Day06 (part1, part2) where

import Data.List (sort, group)
import Data.List.Split (splitOn)

import Paths_advent_of_code

part1 :: IO Int
part1 = sum . map (length . group . sort) <$> readAnswers

part2 :: IO Int
part2 = return 6

readAnswers :: IO [String]
readAnswers = map removeWhitespace . splitOn "\n\n" <$> (readFile =<< getDataFileName "inputs/day06.txt")

removeWhitespace :: String -> String
removeWhitespace = filter (not . (`elem` " \n"))
