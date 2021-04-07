{-# LANGUAGE FlexibleInstances #-}
module Day17
  ( part1
  , part2
  ) where

import           Data.Functor                   ( ($>) )
import qualified Data.Map.Strict               as M
import           Data.Map.Strict                ( (!) )
import           Data.Maybe                     ( mapMaybe )
import           Paths_advent_of_code           ( getDataFileName )

import           Helpers                        ( readInts )


data State = Active | Inactive deriving (Show, Eq)
type Coords3 = (Int, Int, Int)
type Coords4 = (Int, Int, Int, Int)


class (Ord a, Eq a) => Coords a where
  initialCoordinates :: Int -> Int -> a
  neighboringIndices :: a -> [a]


instance Coords Coords3 where
  initialCoordinates x y = (x, y, 0)
  neighboringIndices (x, y, z) =
    [ (a, b, c)
    | a <- [x - 1 .. x + 1]
    , b <- [y - 1 .. y + 1]
    , c <- [z - 1 .. z + 1]
    , (a, b, c) /= (x, y, z)
    ]


instance Coords Coords4 where
  initialCoordinates x y = (x, y, 0, 0)
  neighboringIndices (x, y, z, w) =
    [ (a, b, c, d)
    | a <- [x - 1 .. x + 1]
    , b <- [y - 1 .. y + 1]
    , c <- [z - 1 .. z + 1]
    , d <- [w - 1 .. w + 1]
    , (a, b, c, d) /= (x, y, z, w)
    ]


part1 :: IO Int
part1 = countActiveAfter6Iterations <$> (input :: IO (M.Map Coords3 State))


part2 :: IO Int
part2 = countActiveAfter6Iterations <$> (input :: IO (M.Map Coords4 State))


countActiveAfter6Iterations :: (Coords a) => M.Map a State -> Int
countActiveAfter6Iterations =
  length . M.filter (== Active) . (!! 6) . iterate tick


readLines :: IO [String]
readLines = lines <$> (readFile =<< getDataFileName "inputs/day17.txt")


input :: (Coords a) => IO (M.Map a State)
input = readCubes <$> readLines
 where
  readCubes :: Coords a => [String] -> M.Map a State
  readCubes strings = M.fromList keyValueList
   where
    indexedStrings = zip [0 ..] (map (zip [0 ..]) strings)
    keyValueList   = concatMap
      (\(y, xs) -> map (\(x, char) -> (initialCoordinates x y, toState char)) xs
      )
      indexedStrings

  toState :: Char -> State
  toState char = if char == '#' then Active else Inactive


tick :: (Coords a) => M.Map a State -> M.Map a State
tick cubes = M.mapWithKey (transformCube cubes) . initNeighbors $ cubes
 where
  transformCube :: (Coords a) => M.Map a State -> a -> State -> State
  transformCube cubes coord state = newState
   where
    newState
      | state == Active && activeNeighborCount `notElem` [2, 3] = Inactive
      | state == Inactive && activeNeighborCount == 3 = Active
      | otherwise = state
    activeNeighborCount = length . M.filter (== Active) $ neighborhood
    neighborhood        = neighbors cubes coord

  initNeighbors :: (Coords a) => M.Map a State -> M.Map a State
  initNeighbors cubes = M.foldlWithKey'
    (\acc coord _ -> M.union (neighbors cubes coord) acc)
    cubes
    cubes

  neighbors :: (Coords a) => M.Map a State -> a -> M.Map a State
  neighbors cubes coord = M.fromList $ map
    (\coord -> (coord, M.findWithDefault Inactive coord cubes))
    (neighboringIndices coord)
