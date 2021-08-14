module Three
    (part1
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
getNextCoordinate :: Char -> (Int, Int) -> (Int, Int)

part1 path =
  let coords = Data.List.foldl (\acc c -> getNextCoordinate c (head acc) : acc) [(0,0)] path
      strs = Data.List.map (\(x,y) -> show x ++ "," ++ show y) coords
  in size (fromList strs)

getNextCoordinate c (x,y)
  | c == '^' = (x, y+1)
  | c == '>' = (x+1, y)
  | c == '<' = (x-1, y)
  | c == 'v' = (x, y-1)
  | otherwise = (x,y)