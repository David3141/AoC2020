module Day01
    ( part1
    , part2
    )
where

import           Helpers                        ( readInts )

part1 :: IO Int
part1 = do
    entries <- readInts "inputs/day01.txt"

    let (a, b) = head [ (a, b) | a <- entries, b <- entries, a + b == 2020 ]

    return $ a * b

part2 :: IO Int
part2 = do
    entries <- readInts "inputs/day01.txt"

    let (a, b, c) = head
            [ (a, b, c)
            | a <- entries
            , b <- entries
            , c <- entries
            , a + b + c == 2020
            ]

    return $ a * b * c
