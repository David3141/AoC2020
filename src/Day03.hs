module Day03
    ( part1
    , part2
    )
where

import           Data.List.Split                ( splitOn )

import           Helpers                        ( readInts )

import           Paths_advent_of_code

part1 :: IO Int
part1 = do
    list <- input

    return $ sum
        [ 1 | (lane, index) <- zip list [0, 3 ..], (lane !! index) == '#' ]

part2 :: IO Int
part2 = return 0

input :: IO [String]
input = map cycle . lines <$> (readFile =<< getDataFileName "inputs/day03.txt")
