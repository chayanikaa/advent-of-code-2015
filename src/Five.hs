module Five
    (
    part1,
    part2,
    containsPairCopied,
    containsLetterRepeat,
    ) where

import Data.List

part1 :: [String] -> Int
isGoodString1 :: [String] -> [String] -> String -> Bool
part2 :: [String] -> Int

containsLetterRepeat :: String -> Bool

part1 strs =
  let badStrings = ["ab", "cd", "pq", "xy"]
      goodStrings = map (\c -> [c,c]) ['a'..'z']
  in length $ filter (isGoodString1 badStrings goodStrings) strs

isGoodString1 badStrings goodStrings str =
  not (any (`isInfixOf` str) badStrings) &&
    any (`isInfixOf` str) goodStrings &&
      length (filter (`elem` "aeiou") str) >= 3


-- part2 strs = length $ filter (\str -> (containsLetterRepeat str) && (containsPairCopied "" str)) strs

part2 strs = length $ filter containsPairCopied $ filter containsLetterRepeat strs

containsPairCopied :: String -> Bool 
containsPairCopied (x:y:xs) 
  | [x,y] `isInfixOf` xs = True
  | otherwise = containsPairCopied (y:xs)
containsPairCopied (x:xs) = False
containsPairCopied [] = False

-- If the string contains a letter repeated with one letter between them
containsLetterRepeat str
  | length str < 3 = False
  | head str == str!!2 = True
  | otherwise = containsLetterRepeat $ tail str
