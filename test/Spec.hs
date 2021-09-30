import Test.Hspec

import qualified Data.List

import qualified One
import qualified Two
import qualified Three
import qualified Four
import qualified Five
import qualified Six
import qualified Seven

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
    it "part 2 containsPairCopied example 1" $ do
      Five.containsPairCopied "" "aaa" `shouldBe` False
    it "part 2 containsPairCopied example 1.5" $ do
      Five.containsPairCopied "" "aaaa" `shouldBe` True
    it "part 2 containsPairCopied example xyxy" $ do
      Five.containsPairCopied "" "xyxy" `shouldBe` True
    it "part 2 containsPairCopied example 2" $ do
      Five.containsPairCopied "" "xxyxx" `shouldBe` True
    it "part 2 containsPairCopied example 2.5" $ do
      Five.containsPairCopied "" "aabcdefgaa" `shouldBe` True
    it "part 2 containsPairCopied example 3" $ do
      Five.containsPairCopied "" "ieodomkazucvgmuy" `shouldBe` False
    it "part 2 containsPairCopied example 4" $ do
      Five.containsPairCopied "" "aabcdefgaa" `shouldBe` True
    it "part 2 example 1" $ do
      Five.part2 ["qjhvhtzxzqqjkmpb"] `shouldBe` 1
    it "part 2 example 2" $ do
      Five.part2 ["xxyxx"] `shouldBe` 1
    it "part 2 example 3" $ do
      Five.part2 ["uurcxstgmygtbstg"] `shouldBe` 0
    it "part 2 example 4" $ do
      Five.part2 ["ieodomkazucvgmuy"] `shouldBe` 0
    it "part 2 containsLetterRepeat" $ do
      Five.containsLetterRepeat "aaa" `shouldBe` True
    it "part 2 containsLetterRepeat" $ do
      Five.containsLetterRepeat "uurcxstgmygtbstg" `shouldBe` False
    it "part 2 containsLetterRepeat example xyxy" $ do
      Five.containsLetterRepeat "xyxy" `shouldBe` True
    it "part 2 containsLetterRepeat example 2.5" $ do
      Five.containsLetterRepeat "aabcdefgaa" `shouldBe` False
    -- xit "part 2 input" $ do -- wrong answer
      -- input <- readFile "./inputs/5.txt"
      -- putStrLn $ show (Data.List.map ( Five.containsPairCopied "") (lines input))
      -- Five.part2 (lines input) `shouldBe` 45
  
  describe "Day 6" $ do
    it "decodeInstruction" $ do
      putStrLn $ show (Six.decodeInstruction "turn on 0,0 through 999,999")
    it "part 1 example 1" $ do
      Six.part1 ["turn on 0,0 through 999,999"] `shouldBe` 1000000
    it "part 1 example 2" $ do
      Six.part1 ["turn on 499,499 through 500,500"] `shouldBe` 4
    it "part 1 example 3" $ do
      Six.part1 ["toggle 0,0 through 999,0"] `shouldBe` 1000
    xit "part 1 input" $ do
      input <- readFile "./inputs/6.txt"
      Six.part1 (lines input) `shouldBe` 377891
    it "part 2 example 1" $ do
      Six.part2 ["turn on 0,0 through 0,0"] `shouldBe` 1
    it "part 2 example 2" $ do
      Six.part2 ["toggle 0,0 through 999,999"] `shouldBe` 2000000
    xit "part 2 input" $ do
      input <- readFile "./inputs/6.txt"
      Six.part2 (lines input) `shouldBe` 14110788
  
  describe "Day 7" $ do
    it "readInstructions" $ do
      input <- readFile "./inputs/7.txt"
      putStrLn $ show $ Seven.readInstructions (lines input)
    it "decodeInstruction" $ do
      input <- readFile "./inputs/7.txt"
      Seven.part1 (lines input) "a" `shouldBe` "456"
    it "readInstructions" $ do
      putStrLn $ show (Seven.processInstruction [] (Seven.Instruction "NOT 3" "x"))