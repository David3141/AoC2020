module Day11 (part1, part2) where

import qualified Data.Map.Strict as M
import Paths_advent_of_code

import Helpers (readInts)

type Layout = M.Map (Int, Int) Seat
data Seat = Occupied | Floor | Empty deriving (Eq, Show)

part1 :: IO Int
part1 = run tick1 <$> input


part2 :: IO Int
part2 = run tick2 <$> input


run :: (Layout -> Layout) -> Layout -> Int
run tick =
  length
    .   filter (== Occupied)
    .   M.elems
    .   fst
    .   head
    .   dropWhile (uncurry (/=))
    .   (zip <*> tail)
    .   iterate tick


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


tick1 :: Layout -> Layout
tick1 layout = M.mapWithKey transform layout
 where
  transform (x, y) seat | numOccupiedNeighbors == 0 && seat == Empty = Occupied
                        | numOccupiedNeighbors >= 4 && seat == Occupied = Empty
                        | otherwise = seat
   where
    numOccupiedNeighbors = countOccupiedSeats layout $ directNeighbors (x, y)

    countOccupiedSeats :: Layout -> [(Int, Int)] -> Int
    countOccupiedSeats layout =
      length . filter (== Occupied) . map (findSeat layout)

    directNeighbors :: (Int, Int) -> [(Int, Int)]
    directNeighbors (x, y) =
      [ (a, b)
      | a <- [x - 1, x, x + 1]
      , a >= 0
      , b <- [y - 1, y, y + 1]
      , b >= 0
      , (a, b) /= (x, y)
      ]


tick2 :: Layout -> Layout
tick2 layout = M.mapWithKey transform layout
 where
  transform (x, y) seat | numBlockedDirections == 0 && seat == Empty = Occupied
                        | numBlockedDirections >= 5 && seat == Occupied = Empty
                        | otherwise = seat
   where
    allDirections = map (map (findSeat layout)) $ directions layout (x, y)
    numBlockedDirections = length . filter isDirectionBlocked $ allDirections

    isDirectionBlocked :: [Seat] -> Bool
    isDirectionBlocked []                = False
    isDirectionBlocked (Empty    : _   ) = False
    isDirectionBlocked (Occupied : _   ) = True
    isDirectionBlocked (Floor    : rest) = isDirectionBlocked rest

    directions :: Layout -> (Int, Int) -> [[(Int, Int)]]
    directions layout (x, y) =
      [ zip stayOnCol goUp      -- North
      , zip goRight   goUp      -- North East
      , zip goRight   stayOnRow -- East
      , zip goRight   goDown    -- South East
      , zip stayOnCol goDown    -- South
      , zip goLeft    goDown    -- South West
      , zip goLeft    stayOnRow -- West
      , zip goLeft    goUp      -- North West
      ]
     where
      (maxX, maxY) = fst . head . M.toDescList $ layout
      goLeft       = [x - 1, x - 2 .. 0]
      goRight      = [x + 1 .. maxX]
      goUp         = [y - 1, y - 2 .. 0]
      goDown       = [y + 1 .. maxY]
      stayOnRow    = repeat y
      stayOnCol    = repeat x



findSeat :: Layout -> (Int, Int) -> Seat
findSeat layout (x, y) = M.findWithDefault Floor (x, y) layout
