module Day05 (part1, part2) where

import Data.List (sort)

import Paths_advent_of_code

part1 :: IO Int
part1 = maximum . map seatId <$> readPasses

part2 :: IO Int
part2 = missingSeatId . map seatId <$> readPasses

readPasses :: IO [String]
readPasses = lines <$> (readFile =<< getDataFileName "inputs/day05.txt")

parseRow :: String -> Int
parseRow = binaryPartition (0, 127) ('F', 'B')

parseColumn :: String -> Int
parseColumn = binaryPartition (0, 7) ('L', 'R')

seatId :: String -> Int
seatId str = row * 8 + column
    where
        row = parseRow . take 7 $str
        column = parseColumn . drop 7 $ str

-- We know that the searched seat has direct neighbors. So to find it,
-- we need to sort all IDs and find an occurence of x followed by x + 2.
-- Then our seat is x + 1.
missingSeatId :: [Int] -> Int
missingSeatId seatIds = missingSeatId' (sort seatIds)
    where
    missingSeatId' (x:y:rest) = if y - x == 2
        then x + 1
        else missingSeatId' (y:rest)

binaryPartition :: (Int, Int) -> (Char, Char) -> String -> Int
binaryPartition (min, max) (lowerChar, upperChar) str =
    binaryPartition' str (min, max)
    where
        binaryPartition' :: String -> (Int, Int) -> Int
        binaryPartition' [] (min, max) = min
        binaryPartition' (c:rest) (min, max) = if c == lowerChar
            then binaryPartition' rest (min, max - (max - min) `div` 2 - 1)
            else binaryPartition' rest (min + (max - min) `div` 2 + 1, max)
