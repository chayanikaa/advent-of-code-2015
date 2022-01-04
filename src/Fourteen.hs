module Fourteen
    (
      part1,
      part2,
    ) where

import Data.List
import Data.Maybe
import qualified Data.Vector

-- (Speed, MoveSeconds, RestSeconds)
parseStatement :: String -> (Int, Int, Int)
parseStatement str =
  let parts = words str
  in (read (parts !! 3) :: Int, read (parts !! 6) :: Int, read (parts !! 13) :: Int)

calculateDistance :: (Int, Int, Int) -> Int ->  Int
calculateDistance (speed, move, rest) sec = mul * speed * move + rem * speed
  where rem = min (sec `mod` (move + rest)) move
        mul = sec `div` (move + rest)

part1 :: [String] -> Int
part1 statements =
  let parsedInput = map parseStatement statements
      distances = calculateDistances parsedInput 2503
  in maximum distances

-- part2 :: [String] -> Int
part2 statements =
  let parsedInput = map parseStatement statements
      seconds = [1..2503]
      distances = map (calculateDistances parsedInput) seconds
      maximums = map maximum distances
      maxZipped = zip maximums distances
      indices = map (\(max, all) -> fromJust $ elemIndex max all) maxZipped
      points = sort $ map length $ group $ sort indices
  in last points

calculateDistances :: [(Int, Int, Int)] -> Int -> [Int]
calculateDistances inputs sec = map (`calculateDistance` sec) inputs
