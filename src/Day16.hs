{-# LANGUAGE ViewPatterns #-}

module Day16
  ( part1
  , part2
  ) where

import           Data.List                      ( isPrefixOf
                                                , permutations
                                                , transpose
                                                )
import           Data.List.Split                ( splitOn )

import           Paths_advent_of_code           ( getDataFileName )

type Rule = (String, Int -> Bool)
type Ticket = [Int]


part1 :: IO Int
part1 = do
  (rules, _, tickets) <- input

  return . sum . filter (not . numMatchesAnyRule rules) . concat $ tickets


part2 :: IO Int
part2 = do
  (rules, myTicket, tickets) <- input

  let validTickets = filter (all (numMatchesAnyRule rules)) tickets

  let getMatchingRuleNames nums =
        map fst . filter (\(_, matches) -> all matches nums) $ rules
  let matchingRules = map getMatchingRuleNames (transpose validTickets)
  let validRules =
        concat $ repeatUntilNoChange removeSingletonDups matchingRules

  return $ product . map snd . filter (isPrefixOf "departure" . fst) $ zip
    validRules
    myTicket


input :: IO ([Rule], Ticket, [Ticket])
input = do
  [ruleStrs, myTicketStr, ticketStrs] <-
    splitOn [""] . lines <$> (readFile =<< getDataFileName "inputs/day16.txt")

  let rules    = map parseRule ruleStrs
  let myTicket = map read . splitOn "," . last $ myTicketStr
  let tickets  = map (map read . splitOn ",") . tail $ ticketStrs

  return (rules, myTicket, tickets)


parseRule :: String -> Rule
parseRule rule = (name, numMatchesRule)
 where
  [name        , ranges      ] = splitOn ": " rule
  [range1      , range2      ] = splitOn " or " ranges
  [read -> min1, read -> max1] = splitOn "-" range1
  [read -> min2, read -> max2] = splitOn "-" range2
  numMatchesRule num =
    (min1 <= num && num <= max1) || (min2 <= num && num <= max2)


numMatchesAnyRule :: [Rule] -> Int -> Bool
numMatchesAnyRule rules num = any (`snd` num) rules


-- Example: [["a"], ["a", "b", "c"]]
--       => [["a"], ["b", "c"]]
removeSingletonDups :: [[String]] -> [[String]]
removeSingletonDups rules = newRules
 where
  newRules   = map removeDups rules
  singletons = concat . filter ((== 1) . length) $ rules

  removeDups :: [String] -> [String]
  removeDups [rule] = [rule]
  removeDups rules  = filter (`notElem` singletons) rules


repeatUntilNoChange :: Eq a => (a -> a) -> a -> a
repeatUntilNoChange f x =
  let x' = f x in if x' == x then x else repeatUntilNoChange f x'
