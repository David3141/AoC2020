{-# LANGUAGE ViewPatterns #-}

module Day16 (part1, part2)
where

import Data.List.Split (splitOn)

import Paths_advent_of_code ( getDataFileName )

type Rule = (String, (Int, Int), (Int, Int))
type Ticket = [Int]

part1 :: IO Int
part1 = do
  (rules, _, tickets) <- input

  return . sum . filter (not . numMatchesAnyRule rules) . concat $ tickets


part2 :: IO Int
part2 = return 16


input :: IO ([Rule], Ticket, [Ticket])
input = do
  [ruleStrs, myTicketStr, ticketStrs] <-
    splitOn [""] . lines <$> (readFile =<< getDataFileName "inputs/day16.txt")

  let rules    = map parseRule ruleStrs
  let myTicket = map read . splitOn "," . last $ myTicketStr
  let tickets  = map (map read . splitOn ",") . tail $ ticketStrs

  return (rules, myTicket, tickets)


parseRule :: String -> Rule
parseRule rule = (name, (min1, max1), (min2, max2))
  where
    [name, ranges] = splitOn ": " rule
    [range1, range2] = splitOn " or " ranges
    [read -> min1, read -> max1] = splitOn "-" range1
    [read -> min2, read -> max2] = splitOn "-" range2


numMatchesRule :: Rule -> Int -> Bool
numMatchesRule (_, (min1, max1), (min2, max2)) num =
  (min1 <= num && num <= max1) || (min2 <= num && num <= max2)


numMatchesAnyRule :: [Rule] -> Int -> Bool
numMatchesAnyRule rules num =
  any (`numMatchesRule` num) rules
