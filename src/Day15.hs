module Day15 (part1, part2)
where

import qualified Data.IntMap.Strict as M


type Memory = M.IntMap Int
data GameState = GameState {
  memory :: Memory,
  spokenNumber :: Int,
  turn :: Int
} deriving Show


input :: [Int]
input = [1, 12, 0, 20, 8, 16]


part1 :: IO Int
part1 = return $ nthNum 2020


part2 :: IO Int
part2 = return $ nthNum 30000000


nthNum :: Int -> Int
nthNum n =
  spokenNumber . head . filter ((== n) . turn) . iterate next $ initialGame


initialGame :: GameState
initialGame = GameState { memory   = M.fromList (zip (init input) [1 ..])
                        , spokenNumber  = last input
                        , turn = length input
                        }


next :: GameState -> GameState
next (GameState memory spokenNumber turn) = GameState newMemory nextNum (turn + 1)
 where
  newMemory = M.insert spokenNumber turn memory
  nextNum   = case M.lookup spokenNumber memory of
    Just someTurn -> turn - someTurn
    Nothing       -> 0
