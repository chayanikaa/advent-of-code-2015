module Five
    (
    part1
    ) where

import Data.List

part1 :: [String] -> Int

part1 strs =
  let badStrings = ["ab", "cd", "pq", "xy"]
      goodStrings = map (\c -> [c,c]) ['a'..'z']
  in length $ filter (isGoodString badStrings goodStrings) strs

isGoodString badStrings goodStrings str =
  (not $ any (\bad -> isInfixOf bad str) badStrings) &&
    (any (\good -> isInfixOf good str) goodStrings) &&
      (length $ filter (\c -> any (==c) "aeiou") str) >= 3