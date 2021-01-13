module Day05 (part1, part2) where

import Data.List (sort)

import Paths_advent_of_code

part1 :: IO Int
part1 = maximum . map seatId <$> readPasses

part2 :: IO Int
part2 = missingSeatId . map seatId <$> readPasses

readPasses :: IO [String]
readPasses = lines <$> (readFile =<< getDataFileName "inputs/day05.txt")

seatId :: String -> Int
seatId = parseAsBinary

parseAsBinary :: String -> Int
parseAsBinary =
  foldl (\acc c -> if countsAsOne c then acc * 2 + 1 else acc * 2) 0
    where
    countsAsOne c = c `elem` "BR"

-- We know that the searched seat has direct neighbors. So to find it,
-- we need to sort all IDs and find an occurence of x followed by x + 2.
-- Then our seat is x + 1.
missingSeatId :: [Int] -> Int
missingSeatId seatIds = missingSeatId' (sort seatIds)
  where
  missingSeatId' (x:y:rest)
    | y - x == 2 = x + 1
    | otherwise = missingSeatId' (y:rest)
