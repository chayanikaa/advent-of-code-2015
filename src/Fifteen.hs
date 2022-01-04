module Fifteen
    (
      part1,
      part2,
    ) where

import Data.List
import Data.Maybe
import qualified Data.Vector

fst' (a,_,_,_) = a
snd' (_,b,_,_) = b
thd' (_,_,c,_) = c
fth' (_,_,_,d) = d

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
part1 =
  let sumCombs = getCombinations
      validResults = map calculateScore sumCombs
  in maximum validResults

part2 :: Int
part2 =
  let sumCombs = getCombinations
      validResults = map calculateScoreCals sumCombs
  in maximum validResults

