module Six
    (
    decodeInstruction,
    part1,
    part2,
    ) where

-- first ever time using an algebraic data type, yay!
data Coord = Coord Int Int deriving (Show)

part1 :: [String] -> Int
decodeInstruction :: String -> (Int, Coord, Coord)
-- decodeInstruction :: String -> [String]
executeInstruction1 :: [(Int, Bool)] -> (Int, Coord, Coord) -> [(Int, Bool)]

getRow :: Coord -> Int
getRow (Coord row _) = row

getCol :: Coord -> Int
getCol (Coord _ col) = col

calculateCoord :: Int -> Coord
calculateCoord ind = Coord (ind `div` 1000) (ind `mod` 1000)

includesCoord :: Coord -> Coord -> Coord -> Bool
includesCoord start end coord =
  if ((getRow coord) >= (getRow start) &&
      (getRow coord) <= (getRow end) &&
      (getCol coord) >= (getCol start) &&
      (getCol coord) <= (getCol end)) then True
  else False

replaceSpaces c
  | c == ',' = ' '
  | otherwise = c

toInt :: String -> Int
toInt s = read s :: Int

decodeInstruction str =
  let strOnlySpaces = map replaceSpaces str
      parts = words strOnlySpaces
  in if (parts!!0 == "toggle") then
        (
          3,
          Coord (read (parts!!1)) (read (parts!!2)),
          Coord (read (parts!!4)) (read (parts!!5))
        )
     else 
        (
          if (parts!!1 == "on") then 1 else 2,
          Coord (read (parts!!2)) (read (parts!!3)),
          Coord (read (parts!!5)) (read (parts!!6))
        )

maybeTurn1 :: Int -> Coord -> Coord -> (Int, Bool) -> (Int, Bool)
maybeTurn1 instruction start end (index, oldValue)
  | (includesCoord start end (calculateCoord index)) =
    if (instruction == 1) then (index, True)
    else if (instruction == 2) then (index, False)
    else (index, not oldValue)
  | otherwise = (index, oldValue)

executeInstruction1 initLights (toggle, start, end) =
  map (maybeTurn1 toggle start end) initLights

part1 strs =
  let instructions = map decodeInstruction strs
      initLights = zip [0..999999] (repeat False)
      lights = foldl executeInstruction1 initLights instructions
  -- in lights
  in length $ filter (\(_,value) -> value==True) lights

turn2 :: Int -> Coord -> Coord -> (Int, Int) -> (Int, Int)
turn2 instruction start end (index, oldValue)
  | (includesCoord start end (calculateCoord index)) =
    if (instruction == 1) then (index, oldValue + 1)
    else if (instruction == 2 && oldValue > 0) then (index, oldValue - 1)
    else if (instruction == 2 && oldValue == 0) then (index, oldValue)
    else (index, oldValue + 2)
  | otherwise = (index, oldValue)

executeInstruction2 :: [(Int, Int)] -> (Int, Coord, Coord) -> [(Int, Int)]
executeInstruction2 initLights (toggle, start, end) =
  map (turn2 toggle start end) initLights

part2 :: [String] -> Int
part2 strs =
  let instructions = map decodeInstruction strs
      initLights = zip [0..999999] (repeat 0)
      lights = foldl executeInstruction2 initLights instructions
  -- in lights
  in foldl (\acc (_,value) -> acc + value) 0 lights