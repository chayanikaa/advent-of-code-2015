import Test.Hspec

import qualified Data.List

import qualified One
import qualified Two
import qualified Three
import qualified Four
import qualified Five
import qualified Six
import qualified Seven
import qualified Eight
import qualified Nine
import qualified Ten
import qualified Eleven
import qualified Thirteen
import qualified Fourteen
import qualified Fifteen
import qualified Seventeen
import qualified Eighteen


main :: IO ()
main = hspec $ do
  describe "Day 1" $ do
    it "Day 1: part 1" $
      One.processString1 "(())" 0 `shouldBe` 0
    it "Day 1: part 2, test 1" $
      One.processString2 ")" 0 0 `shouldBe` 1
    it "Day 1: part 2, test 2" $
      One.processString2 "()())" 0 0 `shouldBe` 5
    it "Day 1: part 2, test 3" $ do
      input <- readFile "./inputs/1.txt"
      One.processString2 input 0 0 `shouldBe` 1795

  describe "Day 2" $ do
    it "Day 2: calculate area 1" $
      Two.calculateArea (2, 3, 4) `shouldBe` 58
    it "Day 2: calculate area 2" $
      Two.calculateArea (1, 1, 10) `shouldBe` 43
    it "Day 2: part1 example" $
      Two.part1 "1x1x10" `shouldBe` 43
    it "Day 2: part1 input" $ do
      input <- readFile "./inputs/2.txt"
      Two.part1 input `shouldBe` 1606483
    it "Day 2: part2 example" $
      Two.part2 "1x1x10" `shouldBe` 14
    it "Day 2: part2 input" $ do
      input <- readFile "./inputs/2.txt"
      Two.part2 input `shouldBe` 3842356

  describe "Day 3" $ do
    it "part 1 example 1" $
      Three.part1 ">" `shouldBe` 2
    it "part 1 input" $ do
      input <- readFile "./inputs/3.txt"
      Three.part1 input `shouldBe` 2592
    it "part 2 example 1" $
      Three.part2 "^v" `shouldBe` 3
    it "part 2 example 2" $
      Three.part2 "^v^v^v^v^v" `shouldBe` 11
    it "part 2 input" $ do
      input <- readFile "./inputs/3.txt"
      Three.part2 input `shouldBe` 2360

  describe "Day 4" $ do
    xit "part 1 example 1" $
      Four.part1 "abcdef" "00000" `shouldBe` 609043 -- about 4 seconds
    xit "part 1 input" $
      Four.part1 "iwrupvqb" "00000" `shouldBe` 346386 -- about 4 seconds
    xit "part 2 input" $
      Four.part1 "iwrupvqb" "000000" `shouldBe` 9958218 -- about 82 seconds

  describe "Day 5" $ do
    it "part 1 example 1" $
      Five.part1 ["haegwjzuvuyypxyu"] `shouldBe` 0
    it "part 1 example 2" $
      Five.part1 ["ugknbfddgicrmopn"] `shouldBe` 1
    it "part 1 input" $ do
      input <- readFile "./inputs/5.txt"
      Five.part1 (lines input) `shouldBe` 258
    it "part 2 containsPairCopied example 1" $
      Five.containsPairCopied "aaa" `shouldBe` False
    it "part 2 containsPairCopied example 1.5" $
      Five.containsPairCopied "aaaa" `shouldBe` True
    it "part 2 containsPairCopied example xyxy" $
      Five.containsPairCopied "xyxy" `shouldBe` True
    it "part 2 containsPairCopied example 2" $
      Five.containsPairCopied "xxyxx" `shouldBe` True
    it "part 2 containsPairCopied example 2.5" $
      Five.containsPairCopied "aabcdefgaa" `shouldBe` True
    it "part 2 containsPairCopied example 3" $
      Five.containsPairCopied "ieodomkazucvgmuy" `shouldBe` False
    it "part 2 containsPairCopied example 4" $
      Five.containsPairCopied "aabcdefgaa" `shouldBe` True
    it "part 2 example 1" $
      Five.part2 ["qjhvhtzxzqqjkmpb"] `shouldBe` 1
    it "part 2 example 2" $
      Five.part2 ["xxyxx"] `shouldBe` 1
    it "part 2 example 3" $
      Five.part2 ["uurcxstgmygtbstg"] `shouldBe` 0
    it "part 2 example 4" $
      Five.part2 ["ieodomkazucvgmuy"] `shouldBe` 0
    it "part 2 containsLetterRepeat" $
      Five.containsLetterRepeat "aaa" `shouldBe` True
    it "part 2 containsLetterRepeat" $
      Five.containsLetterRepeat "uurcxstgmygtbstg" `shouldBe` False
    it "part 2 containsLetterRepeat example xyxy" $
      Five.containsLetterRepeat "xyxy" `shouldBe` True
    it "part 2 containsLetterRepeat example 2.5" $
      Five.containsLetterRepeat "aabcdefgaa" `shouldBe` False
    it "part 2 input" $ do
      input <- readFile "./inputs/5.txt"
      Five.part2 (lines input) `shouldBe` 53

  describe "Day 6" $ do
    it "decodeInstruction" $
      putStrLn $ show (Six.decodeInstruction "turn on 0,0 through 999,999")
    it "part 1 example 1" $
      Six.part1 ["turn on 0,0 through 999,999"] `shouldBe` 1000000
    it "part 1 example 2" $
      Six.part1 ["turn on 499,499 through 500,500"] `shouldBe` 4
    it "part 1 example 3" $
      Six.part1 ["toggle 0,0 through 999,0"] `shouldBe` 1000
    xit "part 1 input" $ do
      input <- readFile "./inputs/6.txt"
      Six.part1 (lines input) `shouldBe` 377891
    it "part 2 example 1" $
      Six.part2 ["turn on 0,0 through 0,0"] `shouldBe` 1
    it "part 2 example 2" $
      Six.part2 ["toggle 0,0 through 999,999"] `shouldBe` 2000000
    xit "part 2 input" $ do
      input <- readFile "./inputs/6.txt"
      Six.part2 (lines input) `shouldBe` 14110788

  describe "Day 7" $ do
    it "part 1" $ do
      input <- readFile "./inputs/7.txt"
      Seven.part1 (lines input) "a" `shouldBe` "3176"
    it "part 2" $ do
      input <- readFile "./inputs/7-2.txt"
      Seven.part1 (lines input) "a" `shouldBe` "14710"

  describe "Day 8" $ do
    it "part 1: test example" $ do
      input <- readFile "./inputs/8-ex.txt"
      Eight.part1 (lines input) `shouldBe` 12
    it "part 1" $ do
      input <- readFile "./inputs/8.txt"
      Eight.part1 (lines input) `shouldBe` 1371
    it "part 2: test example" $ do
      input <- readFile "./inputs/8-ex.txt"
      Eight.part2 (lines input) `shouldBe` 19
    it "part 2" $ do
      input <- readFile "./inputs/8.txt"
      Eight.part2 (lines input) `shouldBe` 2117
  
  describe "Day 9" $ do
    it "part 1" $ do
      input <- readFile "./inputs/9.txt"
      Nine.part1 (lines input) `shouldBe` 117
    it "part 2" $ do
      input <- readFile "./inputs/9.txt"
      Nine.part2 (lines input) `shouldBe` 909

  describe "Day 10" $ do
    it "part 1" $ do
      Ten.part1 "1113122113" `shouldBe` 360154
    it "part 2" $ do
      Ten.part2 "1113122113" `shouldBe` 5103798
  
  describe "Day 11" $ do
    -- it "part 1: example 1" $ do
    --   Eleven.part1 "abcdefgh" 0 `shouldBe` "abcdffaa"
    -- it "part 1: example 2" $ do
    --   Eleven.part1 "ghijklmn" 0 `shouldBe` "ghjaabcc"
    it "part 1" $ do
      Eleven.part1 "vzbxkghb" 0 `shouldBe` "vzbxxyzz"
    it "part 2" $ do
      Eleven.part1 "vzbxkghb" 1 `shouldBe` "vzcaabcc"

  describe "Day 13" $ do
    it "part 1" $ do
      input <- readFile "./inputs/13.txt"
      Thirteen.part1 (lines input) `shouldBe` 618
    it "part 2" $ do
      input <- readFile "./inputs/13.txt"
      Thirteen.part2 (lines input) `shouldBe` 601
  
  describe "Day 14" $ do
    it "part 1" $ do
      input <- readFile "./inputs/14.txt"
      Fourteen.part1 (lines input) `shouldBe` 2655
    it "part 2" $ do
      input <- readFile "./inputs/14.txt"
      Fourteen.part2 (lines input) `shouldBe` 1059
    
  describe "Day 14" $ do
    it "part 1" $ do
      input <- readFile "./inputs/14.txt"
      Fourteen.part1 (lines input) `shouldBe` 2655
    it "part 2" $ do
      input <- readFile "./inputs/14.txt"
      Fourteen.part2 (lines input) `shouldBe` 1059

  describe "Day 15" $ do
    it "part 1" $ do
      Fifteen.part1 `shouldBe` 13882464
    it "part 2" $ do
      Fifteen.part2 `shouldBe` 11171160
  
  describe "Day 17" $ do
    it "part 1" $ do
      input <- readFile "./inputs/17.txt"
      Seventeen.part1 (lines input) `shouldBe` 4372
    it "part 2" $ do
      input <- readFile "./inputs/17.txt"
      Seventeen.part2 (lines input) `shouldBe` 4
  
  describe "Day 18" $ do
    it "part 1" $ do
      input <- readFile "./inputs/18.txt"
      Eighteen.part1 (lines input) `shouldBe` 1061
    it "part 1" $ do
      input <- readFile "./inputs/18-2.txt"
      Eighteen.part2 (lines input) `shouldBe` 1061