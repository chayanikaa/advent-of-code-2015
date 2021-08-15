import Test.Hspec

import qualified One
import qualified Two
import qualified Three
import qualified Four
import qualified Five

main :: IO ()
main = hspec $ do
  describe "Day 1" $ do
    it "Day 1: part 1" $ do
      One.processString1 "(())" 0 `shouldBe` 0
    it "Day 1: part 2, test 1" $ do
      One.processString2 ")" 0 0 `shouldBe` 1
    it "Day 1: part 2, test 2" $ do
      One.processString2 "()())" 0 0 `shouldBe` 5
    it "Day 1: part 2, test 3" $ do
      input <- readFile "./inputs/1.txt"
      One.processString2 input 0 0 `shouldBe` 1795
    
  describe "Day 2" $ do
    it "Day 2: calculate area 1" $ do
      Two.calculateArea (2, 3, 4) `shouldBe` 58
    it "Day 2: calculate area 2" $ do
      Two.calculateArea (1, 1, 10) `shouldBe` 43
    it "Day 2: part1 example" $ do
      Two.part1 "1x1x10" `shouldBe` 43
    it "Day 2: part1 input" $ do
      input <- readFile "./inputs/2.txt"
      Two.part1 input `shouldBe` 1606483
    it "Day 2: part2 example" $ do
      Two.part2 "1x1x10" `shouldBe` 14
    it "Day 2: part2 input" $ do
      input <- readFile "./inputs/2.txt"
      Two.part2 input `shouldBe` 3842356

  describe "Day 3" $ do
    it "part 1 example 1" $ do
      Three.part1 ">" `shouldBe` 2
    it "part 1 input" $ do
      input <- readFile "./inputs/3.txt"
      Three.part1 input `shouldBe` 2592
    it "part 2 example 1" $ do
      Three.part2 "^v" `shouldBe` 3
    it "part 2 example 2" $ do
      Three.part2 "^v^v^v^v^v" `shouldBe` 11
    it "part 2 input" $ do
      input <- readFile "./inputs/3.txt"
      Three.part2 input `shouldBe` 2360

  describe "Day 4" $ do
    xit "part 1 example 1" $ do
      Four.part1 "abcdef" "00000" `shouldBe` 609043 -- about 4 seconds
    xit "part 1 input" $ do
      Four.part1 "iwrupvqb" "00000" `shouldBe` 346386 -- about 4 seconds
    xit "part 2 input" $ do
      Four.part1 "iwrupvqb" "000000" `shouldBe` 9958218 -- about 82 seconds

  describe "Day 5" $ do
    it "part 1 example 1" $ do
      Five.part1 ["haegwjzuvuyypxyu"] `shouldBe` 0
    it "part 1 example 2" $ do
      Five.part1 ["ugknbfddgicrmopn"] `shouldBe` 1
    it "part 1 input" $ do
      input <- readFile "./inputs/5.txt"
      Five.part1 (lines input) `shouldBe` 258
    