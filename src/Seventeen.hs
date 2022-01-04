module Seventeen
    (
      part1,
      part2,
    ) where
import Data.List (subsequences)

parseStatement :: String -> Int
parseStatement str = read str :: Int

part1 :: [String] -> Int
part1 statements =
  let containers = map parseStatement statements
      combs = subsequences containers
  in length $ filter (\l -> sum l == 150) combs

part2 :: [String] -> Int
part2 statements =
  let containers = map parseStatement statements
      combs = subsequences containers
      validCombs = filter (\l -> sum l == 150) combs
      minLength = minimum (map length validCombs)
  in length $ filter (\l -> length l == minLength) validCombs
