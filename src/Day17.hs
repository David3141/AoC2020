{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
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
type Coord3 = (Int, Int, Int)
type Coord4 = (Int, Int, Int, Int)

type Cubes3 = M.Map Coord3 State

type Cubes4 = M.Map Coord4 State


part1 :: IO Int
part1 = length . M.filter (== Active) . (!! 6) . iterate tick <$> input
 where
  input :: IO Cubes3
  input = do
    strings <- lines <$> (readFile =<< getDataFileName "inputs/day17.txt")

    let indexedStrings = zip [0 ..] (map (zip [0 ..]) strings)
    let toState char = if char == '#' then Active else Inactive
    let x = concatMap
          (\(y, xs) -> map (\(x, char) -> ((x, y, 0), toState char)) xs)
          indexedStrings

    return $ M.fromList x


  tick :: Cubes3 -> Cubes3
  tick cubes = M.mapWithKey (transformCube cubes) . initNeighbors $ cubes


  transformCube :: Cubes3 -> Coord3 -> State -> State
  transformCube cubes coord state = newState
   where
    newState
      | state == Active && activeNeighborCount `notElem` [2, 3] = Inactive
      | state == Inactive && activeNeighborCount == 3 = Active
      | otherwise = state
    activeNeighborCount = length . M.filter (== Active) $ neighborhood
    neighborhood        = neighbors cubes coord


  neighbors :: Cubes3 -> Coord3 -> Cubes3
  neighbors cubes coord = M.fromList $ map
    (\coord -> (coord, M.findWithDefault Inactive coord cubes))
    (neighboringIndices coord)

   where
    neighboringIndices :: Coord3 -> [Coord3]
    neighboringIndices (x, y, z) =
      [ (a, b, c)
      | a <- [x - 1 .. x + 1]
      , b <- [y - 1 .. y + 1]
      , c <- [z - 1 .. z + 1]
      , (a, b, c) /= (x, y, z)
      ]


  initNeighbors :: Cubes3 -> Cubes3
  initNeighbors cubes = M.foldlWithKey'
    (\acc coord _ -> M.union (neighbors cubes coord) acc)
    cubes
    cubes


part2 :: IO Int
part2 = length . M.filter (== Active) . (!! 6) . iterate tick <$> input
 where
  input :: IO Cubes4
  input = do
    strings <- lines <$> (readFile =<< getDataFileName "inputs/day17.txt")

    let indexedStrings = zip [0 ..] (map (zip [0 ..]) strings)
    let toState char = if char == '#' then Active else Inactive
    let x = concatMap
          (\(y, xs) -> map (\(x, char) -> ((x, y, 0, 0), toState char)) xs)
          indexedStrings

    return $ M.fromList x


  tick :: Cubes4 -> Cubes4
  tick cubes = M.mapWithKey (transformCube cubes) . initNeighbors $ cubes


  transformCube :: Cubes4 -> Coord4 -> State -> State
  transformCube cubes coord state = newState
   where
    newState
      | state == Active && activeNeighborCount `notElem` [2, 3] = Inactive
      | state == Inactive && activeNeighborCount == 3 = Active
      | otherwise = state
    activeNeighborCount = length . M.filter (== Active) $ neighborhood
    neighborhood        = neighbors cubes coord


  neighbors :: Cubes4 -> Coord4 -> Cubes4
  neighbors cubes coord = M.fromList $ map
    (\coord -> (coord, M.findWithDefault Inactive coord cubes))
    (neighboringIndices coord)

   where
    neighboringIndices :: Coord4 -> [Coord4]
    neighboringIndices (x, y, z, w) =
      [ (a, b, c, d)
      | a <- [x - 1 .. x + 1]
      , b <- [y - 1 .. y + 1]
      , c <- [z - 1 .. z + 1]
      , d <- [w - 1 .. w + 1]
      , (a, b, c, d) /= (x, y, z, w)
      ]


  initNeighbors :: Cubes4 -> Cubes4
  initNeighbors cubes = M.foldlWithKey'
    (\acc coord _ -> M.union (neighbors cubes coord) acc)
    cubes
    cubes
