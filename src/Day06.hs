module Day06 (part1, part2) where

import Data.List (sort, group)
import Data.List.Split (splitOn)

import Paths_advent_of_code

part1 :: IO Int
part1 = sum . map countAllUniqueLetters <$> readAnswers

part2 :: IO Int
part2 = sum . map countLettersAppearingInEachLine <$> readAnswers


readAnswers :: IO [String]
readAnswers = splitOn "\n\n" <$> (readFile =<< getDataFileName "inputs/day06.txt")

removeWhitespace :: String -> String
removeWhitespace = filter (not . (`elem` " \n"))

countAllUniqueLetters :: String -> Int
countAllUniqueLetters = length . group . sort . removeWhitespace

countLettersAppearingInEachLine :: String -> Int
countLettersAppearingInEachLine answers =
    length . filter (== numPersons) . countEachLetter $ answers
    where
        countEachLetter = map length . group . sort . removeWhitespace
        numPersons = length . lines $ answers
