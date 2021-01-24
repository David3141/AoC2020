module Day14 (part1, part2) where

import Data.Char (digitToInt)
import Data.List (isPrefixOf, stripPrefix)
import Data.List.Split (splitOn)
import qualified Data.IntMap.Strict as M
import Data.Bits ((.|.), (.&.))
import Numeric (readInt)


import Paths_advent_of_code


type Mask = (Int, Int) -- two masks, one for OR (X = 0), one for AND (X = 1)
type Update = (Int, Int) -- address + value
type Memory = M.IntMap Int


part1 :: IO Int
part1 = solve1 M.empty (0, 0) <$> input


part2 :: IO Int
part2 = return 14


input :: IO [String]
input = lines <$> (readFile =<< getDataFileName "inputs/day14.txt")


solve1 :: Memory -> Mask -> [String] -> Int
solve1 mem _ [] = sum mem
solve1 mem mask (line:rest)
  | "mask =" `isPrefixOf` line = solve1 mem nextMask rest
  | otherwise = solve1 nextMem mask rest
  where
    nextMask = parseMask line
    nextMem = updateMem mask (parseUpdate line) mem


parseMask :: String -> Mask
parseMask line = (orMask, andMask)
  where
    (_, maskStr) = splitAt 7 line
    orMask = readBin False maskStr
    andMask = readBin True maskStr


parseUpdate :: String -> Update
parseUpdate line = (read address, read value)
  where
    [left, value] = splitOn "] = " line
    [_, address] = splitOn "[" left


updateMem :: Mask -> (Int, Int) -> Memory -> Memory
updateMem (orMask, andMask) (address, num) =
  M.alter (const $ Just (num .&. andMask .|. orMask)) address


readBin :: Bool -> String -> Int
readBin xAs1 = fst . head . readInt 2 (const True) charToInt
  where
    charToInt c
      | c `elem` "01" = digitToInt c
      | xAs1 = 1
      | otherwise = 0
