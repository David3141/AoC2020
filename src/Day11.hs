module Day11 (part1, part2) where

import qualified Data.Map.Strict as M
import Paths_advent_of_code

import Helpers (readInts)

type Layout = M.Map (Int, Int) Seat
data Seat = Occupied | Floor | Empty deriving (Eq, Show)

part1 :: IO Int
part1 =
  length
    .   filter (== Occupied)
    .   M.elems
    .   fst
    .   head
    .   dropWhile (uncurry (/=))
    .   (zip <*> tail)
    .   iterate tick
    <$> input


part2 :: IO Int
part2 = return 11


input :: IO Layout
input =
  M.fromList
    .   concatMap parseLine
    .   zip [0 ..]
    .   lines
    <$> (readFile =<< getDataFileName "inputs/day11.txt")
 where
  parseLine (y, line) = map
    (\(x, seat) -> (,) (x, y) $ case seat of
      '#' -> Occupied
      '.' -> Floor
      'L' -> Empty
    )
    (zip [0 ..] line)


tick :: Layout -> Layout
tick layout = M.mapWithKey transform layout
 where
  transform (x, y) seat | numOccupiedNeighbors == 0 && seat == Empty = Occupied
                        | numOccupiedNeighbors >= 4 && seat == Occupied = Empty
                        | otherwise = seat
   where
    numOccupiedNeighbors =
      length
        . filter (== Occupied)
        . map (\(a, b) -> M.findWithDefault Floor (a, b) layout)
        $ neighboringIndices (x, y)


neighboringIndices :: (Int, Int) -> [(Int, Int)]
neighboringIndices (x, y) =
  [ (a, b)
  | a <- [x - 1, x, x + 1]
  , a >= 0
  , b <- [y - 1, y, y + 1]
  , b >= 0
  , (a, b) /= (x, y)
  ]
