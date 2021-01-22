{-# LANGUAGE ViewPatterns #-}

module Day12 (part1, part2) where

import Control.Monad (mapM_)
import Paths_advent_of_code

import Helpers (readInts)


part1 :: IO Int
part1 = do
  (_, (x, y)) <- foldl followInstructions (0, (0, 0)) <$> input

  return $ abs x + abs y


followInstructions :: (Int, (Int, Int)) -> String -> (Int, (Int, Int))
followInstructions (degree, (x, y)) (c : (read -> val)) = case c of
  'N' -> (degree, (x, y - val))
  'S' -> (degree, (x, y + val))
  'E' -> (degree, (x + val, y))
  'W' -> (degree, (x - val, y))
  'L' -> ((degree - val) `rem` 360, (x, y))
  'R' -> ((degree + val) `rem` 360, (x, y))
  'F' -> (degree, moveInDirection degree (x, y) val)


moveInDirection :: Int -> (Int, Int) -> Int -> (Int, Int)
moveInDirection 0      (x, y) val = (x + val, y) -- East
moveInDirection 90     (x, y) val = (x, y + val) -- South
moveInDirection 180    (x, y) val = (x - val, y) -- West
moveInDirection 270    (x, y) val = (x, y - val) -- North
moveInDirection (-90 ) (x, y) val = (x, y - val) -- North
moveInDirection (-180) (x, y) val = (x - val, y) -- West
moveInDirection (-270) (x, y) val = (x, y + val) -- South



part2 :: IO Int
part2 = return 12


input :: IO [String]
input = lines <$> (readFile =<< getDataFileName "inputs/day12.txt")
