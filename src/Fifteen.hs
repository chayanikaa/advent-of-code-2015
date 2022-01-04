module Fifteen
    (
      part1,
      part2,
    ) where

getCombinations :: [(Int, Int, Int, Int)]
getCombinations =
  let massiveArr = [ (a,b,c,d) | a<-[0..100], b<-[0..100], c<-[0..100], d<-[0..100] ]
      validCombs = filter (\(a,b,c,d) -> a+b+c+d==100) massiveArr
  in validCombs

calculateScore :: (Int,Int,Int,Int) -> Int
calculateScore (a,b,c,d) =
  max (5*a + (-1)*b + 0*c + (-1)*d) 0 *
  max ((-1)*a + 3*b + (-1)*c + 0) 0 *
  max (0*a + 0*b + 4*c + 0*d) 0 *
  max (0*a + 0*b + 0*c + 2*d) 0

calculateScoreCals :: (Int,Int,Int,Int) -> Int
calculateScoreCals (a,b,c,d) =
  let score = max (5*a + (-1)*b + 0*c + (-1)*d) 0 *
              max ((-1)*a + 3*b + (-1)*c + 0) 0 *
              max (0*a + 0*b + 4*c + 0*d) 0 *
              max (0*a + 0*b + 0*c + 2*d) 0
      calories = 5*a+1*b+6*c+8*d
  in if calories == 500 then score else 0

part1 :: Int
part1 = maximum $ map calculateScore getCombinations

part2 :: Int
part2 = maximum $ map calculateScoreCals getCombinations
