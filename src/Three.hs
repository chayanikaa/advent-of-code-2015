module Three
    (part1,
     part2,
    ) where

import Data.Set
import Data.List

-- I need a data structure that is somewhat optimized for lookups
-- Simple implementation could use a list right but that's not fast for lookups
-- In JS I would use Map or Object. But apparently hash table like things aren't purely functional. why?
-- Data.Set is possible for part 1 but it's probable that part 2 requires the number of times a house is visited.
-- What about a pair in a list. ew. But what does that look like? [(x, y, num)]
-- How does this solution play out with immutability, In JS I would mutate the same data structure to store more information
-- What if I just pushed a new string based on coordinates every time and just uniquify the list afterwards and returned the length for part 1?

part1 :: String -> Int
part2 :: String -> Int
getNextCoordinate :: Char -> (Int, Int) -> (Int, Int)
splitDirections :: String -> (String, String)

part1 path = size $ fromList $ getAllCoords path

getAllCoords path =
  let coords = Data.List.foldl (\acc c -> getNextCoordinate c (head acc) : acc) [(0,0)] path
  in Data.List.map (\(x,y) -> show x ++ "," ++ show y) coords

getNextCoordinate c (x,y)
  | c == '^' = (x, y+1)
  | c == '>' = (x+1, y)
  | c == '<' = (x-1, y)
  | c == 'v' = (x, y-1)
  | otherwise = (x,y)

-- Part 2, Easiest to split the list in 2 and just execute the algo twice then uniquify?
-- Let's try that.

-- Split even and odd indexed directions
splitDirections dirs =
  let directionsWithIndex = Data.List.zip [0..] dirs -- infinite list, so cool
      (s, u) = Data.List.partition (even . fst) directionsWithIndex
      (_, s2) = Data.List.unzip s
      (_, u2) = Data.List.unzip u
  in (s2, u2)

part2 path =
  let (path1, path2) = splitDirections path
  in size $ fromList (getAllCoords path1 ++ getAllCoords path2)