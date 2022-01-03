module Eleven
    (
    isValid,
    part1,
    ) where

import Data.List
import Data.Char

import Data.Vector

isValid :: Vector Int -> Bool
isValid vecStr = containsRow str &&
              noBannedChars str &&
              containsTwoPairs str
              where str = toList vecStr

containsRow :: [Int] -> Bool
containsRow (x:y:z:xs)    = (z-y == 1 && y-x == 1) || containsRow (y:z:xs)
containsRow (x:xs)            = False
containsRow []                = False

-- lio
noBannedChars :: [Int] -> Bool
noBannedChars str = not (Data.List.elem 108 str || Data.List.elem 105 str || Data.List.elem 111 str)

containsTwoPairs :: [Int] -> Bool
containsTwoPairs str = Data.List.length (Data.List.filter (\l -> Data.List.length l >= 2) (group str)) >=2

nullify :: Int -> Vector Int -> Vector Int
nullify pos str =
    let indList = [0..7]
    in fromList $ Data.List.map (\i -> if i==pos then 97 else str!i) indList

increment :: Int -> Vector Int -> Vector Int
-- increment the last char till z
-- if at z, increment the next available char(if char is z, then it is not available)
-- reset all lesser chars to a
increment (-1) str = error "Cannot increment further"
increment pos str
    | (str ! pos) >= 122 = increment (pos -1) (nullify pos str)
    | otherwise = fromList (Data.List.map (incrementPos pos str) [0..7])

incrementPos :: Int -> Vector Int -> Int -> Int
incrementPos pos vecStr ind
    | ind == pos = (vecStr!ind)+1
    | otherwise = vecStr!ind

part1 :: String -> Int -> String
part1 str ind =
    let numVec = fromList (Data.List.map ord str)
        incList = iterate (increment 7) numVec
        filtered = Data.List.filter isValid incList
    in Data.List.map chr $ toList $ filtered !! ind