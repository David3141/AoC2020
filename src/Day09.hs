module Day09 (part1, part2) where

import Helpers (readInts)

part1 :: IO Int
part1 = firstInvalidNum <$> input

part2 :: IO Int
part2 = do
  numbers <- input

  let invalidNum = firstInvalidNum numbers
  let sequence   = findSequenceSummingToNum invalidNum numbers

  return $ sum [minimum sequence, maximum sequence]

input :: IO [Int]
input = readInts "inputs/day09.txt"

firstInvalidNum :: [Int] -> Int
firstInvalidNum (x : rest) | hasInvalidNum = candidate
                           | otherwise     = firstInvalidNum rest
 where
  (preamble, [candidate]) = splitAt 25 $ take 26 (x : rest)
  matchingPairs = filter (\(x, y) -> x + y == candidate) (pairs preamble)
  hasInvalidNum = null matchingPairs

findSequenceSummingToNum :: Int -> [Int] -> [Int]
findSequenceSummingToNum invalidNum (x : rest)
  | not (null candidates) = head candidates
  | otherwise             = findSequenceSummingToNum invalidNum rest
 where
  sequences  = map (\count -> take count (x : rest)) [2 .. length (x : rest)]
  candidates = filter ((== invalidNum) . sum) sequences

pairs :: Eq a => [a] -> [(a, a)]
pairs list = [ (x, y) | x <- list, y <- list, x /= y ]
