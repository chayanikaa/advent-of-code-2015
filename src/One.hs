module One
    ( processString1, processString2
    ) where

processChar :: Char -> Int -> Int
processString1 :: String -> Int -> Int

main :: IO ()
main = do
      input <- readFile "../inputs/1.txt"
      putStrLn (show (processString1 input 0))
      putStrLn (show (processString2 input 0 0))

processChar char currFloor
  | char == '(' = currFloor + 1
  | otherwise = currFloor - 1

processString1 str currFloor
  | null str = currFloor
  | otherwise = processString1 (tail str) (processChar (head str) currFloor)

processString2 str currFloor pos
  | null str = pos
  | currFloor == -1 = pos
  | otherwise = processString2 (tail str) (processChar (head str) currFloor) (pos + 1)
