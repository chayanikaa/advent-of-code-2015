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
containsPairCopied :: String -> String -> Bool
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

part2 strs = length $ filter (containsPairCopied "") $ filter containsLetterRepeat strs

-- part2 strs = length . filter . isNice2

-- If a string contains a substring of 2 repeated, but not overlapping
containsPairCopied toFind str
  | length str < 2 = False
  | length toFind == 0 = containsPairCopied (take 2 str) (drop 2 str)
  | length str >= 2 = ( toFind == take 2 str )
                  || containsPairCopied toFind (tail str)
                  || containsPairCopied "" str

-- If the string contains a letter repeated with one letter between them
containsLetterRepeat str
  | length str < 3 = False
  | str!!0 == str!!2 = True
  | otherwise = containsLetterRepeat $ tail str
