{-# LANGUAGE ViewPatterns #-}

module Day12 (part1, part2) where

import Control.Monad (mapM_)
import Data.Tuple (swap)
import Paths_advent_of_code

import Helpers (readInts)

type Coords = (Int, Int)

part1 :: IO Int
part1 = do
  (_, (x, y)) <- foldl followInstructions (0, (0, 0)) <$> input

  return $ abs x + abs y


part2 :: IO Int
part2 = do
  ((x, y), _) <- foldl followInstructions2 ((0, 0), (10, -1)) <$> input

  return $ abs x + abs y


followInstructions :: (Int, Coords) -> String -> (Int, Coords)
followInstructions (degree, coords) (c : (read -> val)) = case c of
  'N' -> (degree, goNorth val coords)
  'S' -> (degree, goSouth val coords)
  'E' -> (degree, goEast val coords)
  'W' -> (degree, goWest val coords)
  'L' -> ((degree - val) `rem` 360, coords)
  'R' -> ((degree + val) `rem` 360, coords)
  'F' -> (degree, moveInDirection degree val coords)


moveInDirection :: Int -> Int -> Coords -> Coords
moveInDirection 0      = goEast
moveInDirection 90     = goSouth
moveInDirection 180    = goWest
moveInDirection 270    = goNorth
moveInDirection (-90 ) = goNorth
moveInDirection (-180) = goWest
moveInDirection (-270) = goSouth


followInstructions2 :: (Coords, Coords) -> String -> (Coords, Coords)
followInstructions2 (ship, waypoint) (c : (read -> val)) = case c of
  'N' -> (ship, goNorth val waypoint)
  'S' -> (ship, goSouth val waypoint)
  'E' -> (ship, goEast val waypoint)
  'W' -> (ship, goWest val waypoint)
  'L' -> (ship, rotateWaypoint ((360 - val) `div` 90) waypoint)
  'R' -> (ship, rotateWaypoint (val `div` 90) waypoint)
  'F' -> (moveToWaypoint waypoint val ship, waypoint)


moveToWaypoint :: Coords -> Int -> Coords -> Coords
moveToWaypoint waypoint count ship =
  foldl1 addCoords (ship : replicate count waypoint)


rotateWaypoint :: Int -> Coords -> Coords
rotateWaypoint count = (!! count) . iterate rotateRight
 where
  rotateRight :: Coords -> Coords
  rotateRight (x, y) = (-y, x)


input :: IO [String]
input = lines <$> (readFile =<< getDataFileName "inputs/day12.txt")


goNorth val (x, y) = (x, y - val)
goSouth val (x, y) = (x, y + val)
goWest val (x, y) = (x - val, y)
goEast val (x, y) = (x + val, y)


addCoords :: Coords -> Coords -> Coords
addCoords (x, y) (a, b) = (x + a, y + b)
