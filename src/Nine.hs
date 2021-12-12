module Nine
    (
        part1,
        part2,
    ) where

import Data.List
import Data.Maybe

parseDistances :: [ String ] -> [(String, String, Int)]
parseDistances = map parseDistance

parseDistance :: String -> (String, String, Int)
parseDistance str =
    let parts = words str
    in (head parts, parts!!2, read (parts!!4) :: Int)

getAllPorts :: [(String, String, Int)] -> [String]
getAllPorts distances = dedupe $ foldl getPorts [] distances

getPorts :: [String] -> (String, String, Int) -> [String]
getPorts ports (port1, port2, _) = port1 : ( port2 : ports )

dedupe :: [String] -> [String]
dedupe = map head . group . sort

getPermutations = permutations . getAllPorts

findDistance :: [(String, String, Int)] -> String -> String -> Int
findDistance distances start end = distance $ fromJust $ find (distanceEq start end) distances

distanceEq :: String -> String -> (String, String, Int) -> Bool
distanceEq start end (a, b, _)
    | a == start && b == end = True
    | b == start && a == end = True
    | otherwise = False

distance (_, _, dis) = dis

splitIntoPathParts :: [String] -> [[String]]
splitIntoPathParts (x1:(x2:xs)) = [x1,x2]:splitIntoPathParts (x2:xs)
splitIntoPathParts (x:xs) = []
splitIntoPathParts [] = []

calculateTotalDistance :: [(String, String, Int)] -> [String] -> Int
calculateTotalDistance distances ports =
    let pathParts = splitIntoPathParts ports
        distanceInts = map (\[start, finish] -> findDistance distances start finish) pathParts
    in sum distanceInts

getTotalDistances :: [String] -> [Int]
getTotalDistances distanceStrings =
    let distances = parseDistances distanceStrings
        ports = getAllPorts distances
        pathPermutations = permutations ports
    in map (calculateTotalDistance distances) pathPermutations

part1 :: [String] -> Int
part1 distanceStrings = minimum $ getTotalDistances distanceStrings

part2 :: [String] -> Int
part2 distanceStrings = maximum $ getTotalDistances distanceStrings
