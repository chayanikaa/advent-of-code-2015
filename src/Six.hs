module Six
    (
    decodeInstruction,
    part1
    ) where

-- first ever time using an algebraic data type, yay!
data Coord = Coord Int Int deriving (Show)

part1 :: [String] -> Int
decodeInstruction :: String -> (Bool, Coord, Coord)
-- decodeInstruction :: String -> [String]
executeInstruction :: [(Int, Bool)] -> (Bool, Coord, Coord) -> [(Int, Bool)]

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
  in (
      parts!!1 == "on",
      Coord (read (parts!!2)) (read (parts!!3)),
      Coord (read (parts!!5)) (read (parts!!6))
    )

thd (_,_,x) = x

maybeTurn :: Bool -> Coord -> Coord -> (Int, Bool) -> (Int, Bool)
maybeTurn newValue start end (index, oldValue)
  | (includesCoord start end (calculateCoord index)) = (index, newValue)
  | otherwise = (index, oldValue)

executeInstruction initLights (toggle, start, end) =
  map (maybeTurn toggle start end) initLights

part1 strs =
  let instructions = map decodeInstruction strs
      initLights = zip [0..999999] (repeat False)
      lights = foldl executeInstruction initLights instructions
  -- in lights
  in length $ filter (\(_,value) -> value==True) lights