module Day14 (part1, part2) where

import Data.Char (digitToInt)
import Data.List (isPrefixOf, stripPrefix, subsequences, foldl')
import Data.List.Split (splitOn)
import qualified Data.IntMap.Strict as M
import Data.Bits ((.|.), (.&.), bit, complement, testBit)
import Numeric (readInt)


import Paths_advent_of_code

type Mask = (Int, Int) -- first mask: only 'X' is 1, second mask: only '1' is 1
type Update = (Int, Int) -- address, value
type Memory = M.IntMap Int


part1 :: IO Int
part1 = solve1 M.empty (0, 0) <$> input


part2 :: IO Int
part2 = solve2 M.empty (0, 0) <$> input


input :: IO [String]
input = lines <$> (readFile =<< getDataFileName "inputs/day14.txt")


solve1 :: Memory -> Mask -> [String] -> Int
solve1 mem _ [] = sum mem
solve1 mem mask (line:rest)
  | "mask = " `isPrefixOf` line = solve1 mem nextMask rest
  | otherwise = solve1 nextMem mask rest
  where
    nextMask = parseMask . drop (length "mask = ") $ line
    nextMem = updateMem mask (parseUpdate line) mem


solve2 :: Memory -> Mask -> [String] -> Int
solve2 mem _ [] = sum mem
solve2 mem mask (line:rest)
  | "mask = " `isPrefixOf` line = solve2 mem nextMask rest
  | otherwise = solve2 nextMem mask rest
  where
    nextMask = parseMask . drop (length "mask = ") $ line
    nextMem = updateMem2 mask (parseUpdate line) mem


parseMask :: String -> Mask
parseMask maskStr = (maskX, mask1)
  where
    maskX = readBin 'X' maskStr
    mask1 = readBin '1' maskStr


parseUpdate :: String -> Update
parseUpdate line = (read address, read value)
  where
    [left, value] = splitOn "] = " line
    [_, address] = splitOn "[" left


updateMem :: Mask -> Update -> Memory -> Memory
updateMem (maskX, mask1) (address, num) =
  M.alter (const $ Just (num .&. maskX .|. mask1)) address


updateMem2 :: Mask -> Update -> Memory -> Memory
updateMem2 mask (address, num) memory = newMemory
 where
  newMemory = foldl' (\mem addr -> M.insert addr num mem)
                     memory
                     (floatingAddresses mask address)


readBin :: Char -> String -> Int
readBin charThatIs1 = fst . head . readInt
  2
  (const True)
  (\c -> if c == charThatIs1 then 1 else 0)


readMask :: String -> Mask
readMask line = (maskX, mask1)
 where
  maskX = readBin '1' line
  mask1 = readBin 'X' line


floatingAddresses :: Mask -> Int -> [Int]
floatingAddresses (maskX, mask1) address = map
  -- Reset all X-bits to 0 first, using the complement of the X-mask,
  -- then apply all floatingAddress masks and the 1-mask
  (\floatingAddr -> address .&. complement maskX .|. floatingAddr .|. mask1)
  (floatingMasks maskX)
 where
  -- given a mask representing all locations of X, returns all floating masks
  -- Example: `floatingMasks 5` returns [0, 1, 4, 5]
  -- Explanation: 5 is 101, or X_X, so all floating masks are:
  -- 0: 000 or ___
  -- 1: 001 or __X
  -- 4: 100 or X__
  -- 5: 101 or X_X
  floatingMasks :: Int -> [Int]
  floatingMasks mask =
    map sum $ subsequences [ bit b | b <- [0 .. 35], testBit mask b ]
