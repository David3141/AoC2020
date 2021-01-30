module Day15 (part1, part2)
where

import qualified Data.IntMap.Strict as M


type Memory = M.IntMap Int
data GameState = GameState Memory Int Int deriving Show


input :: [Int]
input = [1, 12, 0, 20, 8, 16]


part1 :: IO Int
part1 = do
  let (GameState _ prevNum _) = (!! (2020 - length input)) play

  return prevNum


play :: [GameState]
play = iterate next (GameState initialMemory prevNum turn)
  where
    prevNum = last input
    turn = length input + 1
    initialMemory = foldl
      (\acc (turn, num) -> M.alter (const $ Just turn) num acc)
      M.empty
      (zip [1 ..] input)


next :: GameState -> GameState
next (GameState memory prevNum turn) = GameState newMemory nextNum (turn + 1)
  where
    newMemory = M.alter (const $ Just (turn - 1)) prevNum memory
    nextNum = case M.lookup prevNum memory of
        Just someTurn -> (turn - 1) - someTurn
        Nothing -> 0


part2 :: IO Int
part2 = return 4

