module Day03
    ( part1
    , part2
    )
where

import           Data.List.Split                ( splitOn )

import           Helpers                        ( readInts )

import           Paths_advent_of_code

part1 :: IO Int
part1 = countTreeEncounters (3, 1) <$> input

part2 :: IO Int
part2 = do
    lanes <- input

    return $ product
        [ countTreeEncounters (1, 1) lanes
        , countTreeEncounters (3, 1) lanes
        , countTreeEncounters (5, 1) lanes
        , countTreeEncounters (7, 1) lanes
        , countTreeEncounters (1, 2) lanes
        ]

input :: IO [String]
input = map cycle . lines <$> (readFile =<< getDataFileName "inputs/day03.txt")

countTreeEncounters :: (Int, Int) -> [String] -> Int
countTreeEncounters (right, down) lanes =
    let numLanes = ceiling $ fromIntegral (length lanes) / fromIntegral down
    in  length
            [ 1
            | (x, y) <- take numLanes $ zip [0, right ..] [0, down ..]
            , ((lanes !! y) !! x) == '#'
            ]
