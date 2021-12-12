module Ten
    (
        part1,
        part2,
    ) where

import Data.List

lookAndSay :: String -> String
lookAndSay = concatMap (\x -> show (length x) ++ [head x]) . group

part1 :: String -> Int
part1 = length . (!! 40) . iterate lookAndSay

part2 :: String -> Int
part2 = length . (!! 50) . iterate lookAndSay