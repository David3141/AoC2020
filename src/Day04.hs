{-# LANGUAGE LambdaCase #-}

module Day04 (part1, part2) where

import Control.Monad (replicateM)
import Data.Maybe (fromMaybe, isJust)
import Data.Char (isAlphaNum, isDigit)
import Data.Foldable (asum)
import Data.List (delete, groupBy)
import Data.List.Split (splitOn)
import Paths_advent_of_code
import Text.Read (readMaybe)
import Text.Regex.Applicative
import qualified Data.Map.Strict as M

type Passport = M.Map String String

data Height = Cm Int
            | In Int
            deriving Show

part1 :: IO Int
part1 = countBy hasAllRequiredEntries <$> readPassports
    where
        hasAllRequiredEntries :: Passport -> Bool
        hasAllRequiredEntries passport =
            all (`M.member` passport) requiredEntry

        requiredEntry = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

part2 :: IO Int
part2 = countBy allRequiredKeysAreValid <$> readPassports
    where
        allRequiredKeysAreValid :: Passport -> Bool
        allRequiredKeysAreValid passport = all (== True)
            [ memberMatchesCondition "byr" byr passport
            , memberMatchesCondition "iyr" iyr passport
            , memberMatchesCondition "eyr" eyr passport
            , memberMatchesCondition "hgt" hgt passport
            , memberMatchesCondition "hcl" hcl passport
            , memberMatchesCondition "ecl" ecl passport
            , memberMatchesCondition "pid" pid passport
            ]

        byr = parseAndCheck int (between 1920 2002)
        iyr = parseAndCheck int (between 2010 2020)
        eyr = parseAndCheck int (between 2020 2030)
        hgt = parseAndCheck parseHeight (\case Cm val -> between 150 193 val
                                               In val -> between 59 76 val)
        hcl = matchesRegex parseHcl
        ecl = matchesRegex parseEcl
        pid = matchesRegex (replicateM 9 $ psym isDigit)



parseHeight :: RE Char Height
parseHeight = parseCm <|> parseInches
    where
        parseCm = Cm <$> int <* string "cm"
        parseInches = In <$> int <* string "in"

parseHcl :: RE Char String
parseHcl = sym '#' *> replicateM 6 (anySymOf "0123456789abcdef")

parseEcl :: RE Char String
parseEcl = string "amb"
    <|> string "blu"
    <|> string "brn"
    <|> string "gry"
    <|> string "grn"
    <|> string "hzl"
    <|> string "oth"

anySymOf :: String -> RE Char Char
anySymOf = asum . map sym

between :: Int -> Int -> Int -> Bool
between lo hi val = val >= lo && val <= hi

regexRule :: RE Char a -> (String -> Bool)
regexRule re = isJust . match re

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


parseAndCheck :: RE Char a -> (a -> Bool) -> String -> Bool
parseAndCheck re cond = maybe False cond . match re

matchesRegex :: RE Char a -> String -> Bool
matchesRegex re = isJust . match re

int :: RE Char Int
int = read <$> many (psym isDigit)

countBy :: (a -> Bool) -> [a] -> Int
countBy cond [] = 0
countBy cond (x : xs)
  | cond x = countBy cond xs + 1
  | otherwise = countBy cond xs

memberMatchesCondition :: String -> (String -> Bool) -> Passport -> Bool
memberMatchesCondition key cond passport =
    case M.lookup key passport of
        Nothing -> False
        Just value -> cond value
