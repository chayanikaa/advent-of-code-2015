module Eighteen
    (
      part1,
      part2,
    ) where

import Data.List
import Data.Maybe
import Data.Vector (Vector, imap, fromList, map, toList,(!))

nextValue :: Int -> Int -> Int
nextValue 1 2 = 1
nextValue 1 3 = 1
nextValue 1 y = 0
nextValue 0 3 = 1
nextValue 0 y = 0

getNeighbors :: Vector ( Vector Int ) -> Int -> Int -> [Int]
getNeighbors grid iRow iCol =
  let maxRow = length grid
      maxCol = length $ grid ! 0
      xs = [iCol-1..iCol+1]
      ys = [iRow-1..iRow+1]
      ps = [(nx, ny) |
         nx <- xs,
         nx >= 0 && nx < maxCol,
         ny <- ys,
         ny >= 0 && ny < maxRow,
         nx /= iCol || ny /= iRow]
  in Data.List.map (\(x,y)->grid!y!x) ps

computeNextStatePoint :: Vector ( Vector Int ) -> Int -> Int -> Int -> Int
computeNextStatePoint grid iRow iCol val =
  let neighbors = getNeighbors grid iRow iCol
  in nextValue val (sum neighbors)

computeNextStateRow :: Vector ( Vector Int ) -> Int -> Vector Int -> Vector Int
computeNextStateRow grid iRow = imap (computeNextStatePoint grid iRow)

computeNextState :: Vector ( Vector Int ) -> Vector ( Vector Int )
computeNextState grid = imap (computeNextStateRow grid) grid

toNum :: String -> [Int]
toNum = Data.List.map (\c -> if c == '#' then 1 else 0)

part1 :: [String] -> Int
part1 statements =
  let parsedInputList = Data.List.map toNum statements
      parsedInputVector = Data.Vector.fromList $ Data.List.map Data.Vector.fromList parsedInputList
      states = iterate computeNextState parsedInputVector
      hundredthState = states !! 100
      statesAsList = toList $ Data.Vector.map Data.Vector.toList hundredthState
  in sum $ Data.List.map sum statesAsList

computeNextStatePoint2 :: Vector ( Vector Int ) -> Int -> Int -> Int -> Int
computeNextStatePoint2 grid iRow iCol val
  | iRow == 0 && iCol == 0 = 1
  | iRow == (length grid - 1) && iCol == 0 = 1
  | iRow == 0 && iCol == (length (grid!0) -  1) = 1
  | iRow == (length grid - 1) && iCol == (length (grid!0) - 1) = 1
  | otherwise =
    let neighbors = getNeighbors grid iRow iCol
    in nextValue val (sum neighbors)

computeNextStateRow2 :: Vector ( Vector Int ) -> Int -> Vector Int -> Vector Int
computeNextStateRow2 grid iRow = imap (computeNextStatePoint2 grid iRow)

computeNextState2 :: Vector ( Vector Int ) -> Vector ( Vector Int )
computeNextState2 grid = imap (computeNextStateRow2 grid) grid

part2 :: [String] -> Int
part2 statements =
  let parsedInputList = Data.List.map toNum statements
      parsedInputVector = Data.Vector.fromList $ Data.List.map Data.Vector.fromList parsedInputList
      states = iterate computeNextState2 parsedInputVector
      hundredthState = states !! 100
      statesAsList = toList $ Data.Vector.map Data.Vector.toList hundredthState
  in sum $ Data.List.map sum statesAsList
