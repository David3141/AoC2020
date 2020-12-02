module Day02
    ( part1
    , part2
    )
where

import           Data.List.Split                ( splitOn )

import           Helpers                        ( readInts )

import           Paths_advent_of_code

part1 :: IO Int
part1 = countElem True . map passwordMatchesOldRule <$> input

part2 :: IO Int
part2 = countElem True . map passwordMatchesNewRule <$> input

input :: IO [String]
input = lines <$> (readFile =<< getDataFileName "inputs/day02.txt")


passwordMatchesOldRule :: String -> Bool
passwordMatchesOldRule ruleAndPassword =
    minCount <= actualCount && actualCount <= maxCount
  where
    (rule     : password   : _) = splitOn ": " ruleAndPassword
    (counts   : (char : _) : _) = splitOn " " rule
    (minCount : maxCount   : _) = map read $ splitOn "-" counts :: [Int]
    actualCount                 = countElem char password


passwordMatchesNewRule :: String -> Bool
passwordMatchesNewRule ruleAndPassword =
    (charIsAtFirstPos && not charIsAtSecondPos)
        || (not charIsAtFirstPos && charIsAtSecondPos)
  where
    (rule   : password   : _) = splitOn ": " ruleAndPassword
    (counts : (char : _) : _) = splitOn " " rule
    (firstIndex : secondIndex : _) =
        map (pred . read) . splitOn "-" $ counts :: [Int]

    charIsAtFirstPos  = password !! firstIndex == char
    charIsAtSecondPos = password !! secondIndex == char


countElem :: Eq a => a -> [a] -> Int
countElem x = length . filter (== x)
