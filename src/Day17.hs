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


type Coord = (Int, Int, Int)
data State = Active | Inactive deriving (Show, Eq)
type Cubes = M.Map Coord State


part1 :: IO Int
part1 = length . M.filter (== Active) . (!! 6) . iterate tick <$> input


part2 :: IO Int
part2 = return 17


input :: IO Cubes
input = do
  strings <- lines <$> (readFile =<< getDataFileName "inputs/day17.txt")

  let indexedStrings = zip [0 ..] (map (zip [0 ..]) strings)
  let toState char = if char == '#' then Active else Inactive
  let x = concatMap
        (\(y, xs) -> map (\(x, char) -> ((x, y, 0), toState char)) xs)
        indexedStrings

  return $ M.fromList x


tick :: Cubes -> Cubes
tick cubes = M.mapWithKey (transformCube cubes) . initNeighbors $ cubes


transformCube :: Cubes -> Coord -> State -> State
transformCube cubes coord state = newState
 where
  newState | state == Active && activeNeighborCount `notElem` [2, 3] = Inactive
           | state == Inactive && activeNeighborCount == 3 = Active
           | otherwise = state
  activeNeighborCount = length . M.filter (== Active) $ neighborhood
  neighborhood        = neighbors cubes coord


neighbors :: Cubes -> Coord -> Cubes
neighbors cubes coord = M.fromList $ map
  (\coord -> (coord, M.findWithDefault Inactive coord cubes))
  (neighboringIndices coord)

 where
  neighboringIndices :: Coord -> [Coord]
  neighboringIndices (x, y, z) =
    [ (a, b, c)
    | a <- [x - 1 .. x + 1]
    , b <- [y - 1 .. y + 1]
    , c <- [z - 1 .. z + 1]
    , (a, b, c) /= (x, y, z)
    ]


initNeighbors :: Cubes -> Cubes
initNeighbors cubes = M.foldlWithKey'
  (\acc coord _ -> M.union (neighbors cubes coord) acc)
  cubes
  cubes
