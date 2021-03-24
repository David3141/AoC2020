{-# LANGUAGE ViewPatterns #-}

module Day08 (part1, part2) where

import Data.List (foldl')
import Data.Foldable (toList)
import qualified Data.Sequence as S

import Paths_advent_of_code

data Command = Acc Int
             | Jmp Int
             | Nop Int
             deriving Show

part1 :: IO Int
part1 = fst . run <$> readLines


part2 :: IO Int
part2 = do
  commands <- readLines

  let indexedCommands = zip [0..] (toList commands)
  let allPossibleCmds = foldl'
        (\result (idx, cmd) -> case cmd of
          Jmp x -> result ++ [S.update idx (Nop x) commands]
          Nop x -> result ++ [S.update idx (Jmp x) commands]
          _     -> result
        )
        [commands]
        indexedCommands

  return $ fst . head . filter snd . map run $ allPossibleCmds


run :: S.Seq Command -> (Int, Bool)
run commands = run' (0, 0, []) (commands `S.index` 0)
  where
  run' :: (Int, Int, [Int]) -> Command -> (Int, Bool) -- (acc, finishedNormally)
  run' (acc, idx, visited) cmd
    | wouldFinishNormally = (nextAcc, True)
    | nextIdx `elem` visited = (nextAcc, False)
    | otherwise = run' (nextAcc, nextIdx, idx:visited) nextCommand
    where
      nextIdx = case cmd of
        Jmp x -> idx + x
        _     -> idx + 1
      nextAcc = case cmd of
        Acc x -> acc + x
        _     -> acc
      wouldFinishNormally = nextIdx > (S.length commands - 1)
      nextCommand = commands `S.index` nextIdx


readLines :: IO (S.Seq Command)
readLines =
  S.fromList . map parseLine . lines
    <$> (readFile =<< getDataFileName "inputs/day08.txt")
  where
  parseLine :: String -> Command
  parseLine ('a':'c':'c':' ':(readSigned -> num)) = Acc num
  parseLine ('j':'m':'p':' ':(readSigned -> num)) = Jmp num
  parseLine ('n':'o':'p':' ':(readSigned -> num)) = Nop num


readSigned :: String -> Int
readSigned ('+':x) = read x
readSigned x = read x
