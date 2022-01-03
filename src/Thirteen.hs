module Thirteen
    (
      part1,
      part2,
    ) where

import Data.List
import Data.Maybe

thd (_,_,h) = h

parseStatement :: String -> (String, String, Int)
parseStatement str =
  let parts = words $ init str
      absHappiness = (read (parts !! 3) :: Int)
      happiness = if "lose" `elem` parts then negate absHappiness else absHappiness
  in (head parts, last parts, happiness)

findHappiness :: [(String, String, Int)] -> String -> String -> Int
findHappiness _ "Me" _ = 0
findHappiness _ _ "Me" = 0
findHappiness input x y = thd $ fromJust $ find (\(a,b,_)-> a==x && b==y) input

calculateHappinessInternal :: [(String, String, Int)] -> [String] -> Int
calculateHappinessInternal input (x:y:xs) = findHappiness input x y + findHappiness input y x + calculateHappinessInternal input (y:xs)
calculateHappinessInternal input (x:xs) = 0
calculateHappinessInternal input [] = 0

calculateHappiness :: [(String, String, Int)] -> [String] -> Int
calculateHappiness input arrangement  = calculateHappinessInternal input arrangement
                                         + calculateHappinessInternal input [head arrangement, last arrangement]

dedupe :: [String] -> [String]
dedupe = map head . group . sort

getAllPeople :: [(String, String, Int)] -> [String]
getAllPeople distances = dedupe $ foldl getPeople [] distances

getPeople :: [String] -> (String, String, Int) -> [String]
getPeople persons (person1, person2, _) = person1 : ( person2 : persons )

part1 :: [String] -> Int
part1 statements =
  let parsedInput = map parseStatement statements
      people = getAllPeople parsedInput
      peoplePerms = permutations people
      happinesses = map (calculateHappiness parsedInput) peoplePerms
  in maximum happinesses
  -- in parsedInput

part2 :: [String] -> Int
part2 statements =
  let parsedInput = map parseStatement statements
      people = "Me" : getAllPeople parsedInput
      peoplePerms = permutations people
      happinesses = map (calculateHappiness parsedInput) peoplePerms
  in maximum happinesses