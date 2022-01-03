{-# LANGUAGE FlexibleContexts#-}

module Two
    (calculateArea,
    parseSingleLine,
    part1,
    part2,
    ) where

import Data.List
import Data.List.Split

import Text.Read
import Data.Maybe (fromMaybe)

calculateArea :: (Int, Int, Int) -> Int
part1 :: String -> Int
parseInput :: String -> [(Int, Int, Int)]
parseSingleLine :: String -> (Int, Int, Int)
generateSides :: (Int, Int, Int) -> [(Int, Int)]
generatePerimeters :: [(Int, Int)] -> [Int]
calculateVolume :: (Int, Int, Int) -> Int
calculateRibbon :: (Int, Int, Int) -> Int

calculateArea (l, w, h) =
    let sides = [l*w, w*h, h*l]
    in (sum (map (2*) sides) + minimum sides)

parseInput str =
    map parseSingleLine (lines str)

parseSingleLine str =
    let maybeInts = map readMaybe (splitOn "x" str) :: [Maybe Int]
        items = map (fromMaybe 0) maybeInts -- Super silent error prone
    in (items!!0, items!!1, items!!2)

part1 str =
    let parsedInputs = parseInput str
        areas = map calculateArea parsedInputs
    in sum areas

generateSides (l,w,h) = [(l,w),(l,h),(w,h)] -- must be a better way to do this

generatePerimeters = map (\(x, y) -> 2 * x + 2 * y)

calculateVolume (l,w,h) = l*w*h

calculateRibbon (l,w,h) = calculateVolume (l,w,h) + (minimum . generatePerimeters . generateSides) (l,w,h)

part2 str =
    let parsedInputs = parseInput str
        lengths = map calculateRibbon parsedInputs
    in sum lengths
