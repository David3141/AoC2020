module Day07 (part1, part2) where

import Data.Char (isDigit)
import Data.Maybe (fromJust, fromMaybe)
import Data.Tuple (swap)
import qualified Data.Map.Strict as M

import Text.Regex.Applicative

import Data.List.Split (splitOn)

import Paths_advent_of_code

type Color = String
type BagRule = (Color, [(Color, Int)])
type BagRules = M.Map Color [(Color, Int)]

part1 :: IO Int
part1 = countBagsContainingShinyGold <$> readRules

part2 :: IO Int
part2 = countNumOfContainedBags "shiny gold" <$> readRules

readRules :: IO BagRules
readRules = M.fromList . map parseBag . lines <$> (readFile =<< getDataFileName "inputs/day07.txt")

parseBag :: String -> BagRule
parseBag = fromJust . match parseRule

parseRule :: RE Char BagRule
parseRule = (,) <$> mainColor <*> many innerBag
  where
  mainColor = many anySym <* string " bags contain" <* noBags
  innerBag = swap <$> ((,) <$> count <*> color)
  count = read <$> (sym ' ' *> some (psym isDigit) <* sym ' ')
  color = few anySym <* string " bag" <* few anySym
  noBags = few (string " no other bags.")

countBagsContainingShinyGold :: BagRules -> Int
countBagsContainingShinyGold bagRules = length $
  M.foldlWithKey
    (\acc key val -> if containsShinyGold val bagRules then key:acc else acc)
    []
    bagRules

containsShinyGold :: [(Color, Int)] -> BagRules -> Bool
containsShinyGold [] _ = False
containsShinyGold ((currentColor,_):rest) bagRules =
  (currentColor == "shiny gold")
  || containsShinyGold lookupOtherBags bagRules
  || containsShinyGold rest bagRules
    where
    lookupOtherBags = fromMaybe [] (M.lookup currentColor bagRules)

countNumOfContainedBags :: Color -> BagRules -> Int
countNumOfContainedBags color bagRules = foldl
  (\acc (key, val) -> acc + val + val * countNumOfContainedBags key bagRules)
  0
  innerBags
    where
    innerBags = fromMaybe [] (M.lookup color bagRules)
