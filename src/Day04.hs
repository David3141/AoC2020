module Day04 (part1, part2) where

import Data.Maybe (fromMaybe)
import Data.Char (isAlphaNum)
import Data.List (delete, groupBy)
import Data.List.Split (splitOn)
import Paths_advent_of_code
import Text.Regex.Applicative
import qualified Data.Map.Strict as M

type Passport = M.Map String String

part1 :: IO Int
part1 = countBy hasAllRequiredEntries <$> readPassports
    where
        hasAllRequiredEntries :: Passport -> Bool
        hasAllRequiredEntries passport =
            all (`M.member` passport) requiredEntry

        requiredEntry = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

part2 :: IO Int
part2 = return 4

readPassports :: IO [Passport]
readPassports =
    map parsePassport . splitOn "\n\n"
    <$> (readFile =<< getDataFileName "inputs/day04.txt")

parsePassport :: String -> Passport
parsePassport = M.fromList . fromMaybe [] . match passport
    where
        passport = many entry           :: RE Char [(String, String)]
        entry = (,) <$> key <*> value   :: RE Char (String, String)
        key = whitespace *> many (psym (/= ':'))
        value = sym ':' *> nonWhitespace <* whitespace
        whitespace = many (psym isWhitespace)
        nonWhitespace = many (psym (not . isWhitespace))
        isWhitespace = (`elem` " \n\t")

countBy :: (a -> Bool) -> [a] -> Int
countBy cond [] = 0
countBy cond (x : xs)
  | cond x = countBy cond xs + 1
  | otherwise = countBy cond xs
