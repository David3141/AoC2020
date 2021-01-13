{-# LANGUAGE LambdaCase #-}

module Day04 (part1, part2) where

import Control.Monad (replicateM)
import Data.Maybe (fromMaybe, isJust)
import Data.Char (isDigit)
import Data.Foldable (asum)
import Data.List.Split (splitOn)
import Paths_advent_of_code
import Text.Regex.Applicative
import qualified Data.Map.Strict as M

type Passport = M.Map String String

data Height = Cm Int
            | In Int
            deriving Show

part1 :: IO Int
part1 = length . filter hasAllRequiredEntries <$> readPassports
  where
    hasAllRequiredEntries :: Passport -> Bool
    hasAllRequiredEntries passport = all
      (`M.member` passport)
      ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

part2 :: IO Int
part2 = length . filter allRequiredKeysAreValid <$> readPassports
  where
    allRequiredKeysAreValid :: Passport -> Bool
    allRequiredKeysAreValid passport =
      all ((== True) . checkRule passport) rules

    checkRule :: Passport -> (String, String -> Bool) -> Bool
    checkRule passport (key, rule) = maybe False rule passportValue
      where passportValue = M.lookup key passport

rules :: [(String, String -> Bool)]
rules =
  [ ("byr", parseAndCheck parseInt (\x -> x >= 1920 && x <= 2002))
  , ("iyr", parseAndCheck parseInt (\x -> x >= 2010 && x <= 2020))
  , ("eyr", parseAndCheck parseInt (\x -> x >= 2020 && x <= 2030))
  , ("hgt", parseAndCheck parseHeight validateHeight)
  , ("hcl", matchesRegex parseHcl)
  , ("ecl", matchesRegex parseEcl)
  , ("pid", matchesRegex (replicateM 9 $ psym isDigit))
  ]
  where
    parseAndCheck :: RE Char a -> (a -> Bool) -> String -> Bool
    parseAndCheck re cond = maybe False cond . match re

    parseInt :: RE Char Int
    parseInt = read <$> many (psym isDigit)

    parseHeight :: RE Char Height
    parseHeight = parseCm <|> parseInches
        where
            parseCm = Cm <$> parseInt <* string "cm"
            parseInches = In <$> parseInt <* string "in"

    validateHeight :: Height -> Bool
    validateHeight (Cm val) = val >= 150 && val <= 193
    validateHeight (In val) = val >= 59 && val <= 76

    parseHcl :: RE Char String
    parseHcl = sym '#' *> replicateM 6 (anySymOf "0123456789abcdef")
      where
      anySymOf :: String -> RE Char Char
      anySymOf = asum . map sym

    parseEcl :: RE Char String
    parseEcl = string "amb"
        <|> string "blu"
        <|> string "brn"
        <|> string "gry"
        <|> string "grn"
        <|> string "hzl"
        <|> string "oth"

    matchesRegex :: RE Char a -> String -> Bool
    matchesRegex re = isJust . match re


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
