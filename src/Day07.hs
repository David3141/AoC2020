module Day07 (part1, part2) where

import Data.Char (isDigit)
import Data.Maybe (fromJust, fromMaybe)
import Data.Tuple (swap)
import qualified Data.Map.Strict as M

import Text.Regex.Applicative

import Data.List.Split (splitOn)

import Paths_advent_of_code

type Color = String
type OuterBag = (Color, [(Color, Int)])
type OuterBags = M.Map Color [(Color, Int)]

part1 :: IO Int
part1 = countBagsContainingColor <$> readRules

part2 :: IO Int
part2 = countNumOfContainedBags "shiny gold" <$> readRules

readRules :: IO OuterBags
readRules = M.fromList . map parseBag . lines <$> (readFile =<< getDataFileName "inputs/day07.txt")

parseBag :: String -> OuterBag
parseBag = fromJust . match parseRule

parseRule :: RE Char OuterBag
parseRule = (,) <$> mainColor <*> many innerBag
    where
    mainColor = many anySym <* string " bags contain" <* noBags
    innerBag = swap <$> ((,) <$> count <*> color)
    count = read <$> (sym ' ' *> some (psym isDigit) <* sym ' ')
    color = few anySym <* string " bag" <* few anySym
    noBags = few (string " no other bags.")

countBagsContainingColor :: OuterBags -> Int
countBagsContainingColor outerBags = length $
    M.foldlWithKey
        (\acc key val -> if containsColor val outerBags then key:acc else acc)
        []
        outerBags

containsColor :: [(Color, Int)] -> OuterBags -> Bool
containsColor [] _ = False
containsColor ((currentColor,_):rest) outerBags =
    (currentColor == "shiny gold")
    || containsColor lookupOtherBags outerBags
    || containsColor rest outerBags
    where
        lookupOtherBags = fromMaybe [] (M.lookup currentColor outerBags)

countNumOfContainedBags :: Color -> OuterBags -> Int
countNumOfContainedBags color outerBags = foldl
    (\acc (key, val) -> acc + val + val * countNumOfContainedBags key outerBags)
    0
    innerBags
    where
        innerBags = fromMaybe [] (M.lookup color outerBags)
