{-# LANGUAGE ViewPatterns #-}

module Day08 (part1, part2) where

import Text.Regex.Applicative

import Paths_advent_of_code

data Command = Acc Int
             | Jmp Int
             | Nop
             deriving Show

part1 :: IO Int
part1 = do
  commands <- readLines
  return $ run commands (0, 0, []) (head commands)

part2 :: IO Int
part2 = return 8

run :: [Command] -> (Int, Int, [Int]) -> Command -> Int
run commands (acc, i, visited) (Acc x) = run commands (acc + x, i + 1, i:visited) (commands !! (i + 1))
run commands (acc, i, visited) Nop = run commands (acc, i + 1, i:visited) (commands !! (i + 1))
run commands (acc, i, visited) (Jmp x)
    | i + x `elem` visited = acc
    | otherwise = run commands (acc, i + x, i:visited) (commands !! (i + x))

readLines :: IO [Command]
readLines = map parseLine . lines <$> (readFile =<< getDataFileName "inputs/day08.txt")
  where
  parseLine :: String -> Command
  parseLine ('a':'c':'c':' ':'+':(read -> num)) = Acc num
  parseLine ('a':'c':'c':' ':'-':(read -> num)) = Acc (negate num)
  parseLine ('j':'m':'p':' ':'+':(read -> num)) = Jmp num
  parseLine ('j':'m':'p':' ':'-':(read -> num)) = Jmp (negate num)
  parseLine ('n':'o':'p':_) = Nop

