{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Map          (fromList)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Data.List         (intersperse)

import Day01 as D1 (checksum, checksum2)
import Day02 as D2 (checksum, checksum2)
import Day03 as D3 (distance)
import Day04 as D4 (hasNoDuplicates, hasNoAnagrams)
import Day05 as D5 (getTerminationIndex)
import Day06 as D6 (cyclesBeforeLoop)
import Day07 as D7 (findRoot, findCorrectWeight)
import Day08 as D8 (largestRegister, run)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "Advent of Code 2017" $ do
  describe "Day 01 (1)" $ do
    it "Two separate matches" $
      D1.checksum "1122" `shouldBe` 3
    it "Four numbers matching each other" $
      D1.checksum "1111" `shouldBe` 4
    it "No matches" $
      D1.checksum "1234" `shouldBe` 0
    it "Cyclical property" $
      D1.checksum "91212129" `shouldBe` 9
  describe "Day 01 (2)" $ do
    it "Two separate matches" $
      D1.checksum2 "1212" `shouldBe` 6
    it "No matches" $
      D1.checksum2 "1221" `shouldBe` 0
    it "One match" $
      D1.checksum2 "123425" `shouldBe` 4
    it "All matches" $
      D1.checksum2 "123123" `shouldBe` 12
    it "Two more matches" $
      D1.checksum2 "12131415" `shouldBe` 4
  describe "Day 02 (1)" $ do
    it "Simple matrix" $
      D2.checksum (map (intersperse '\t') ["5195",
                                           "753",
                                           "2468"])
      `shouldBe` 18
  describe "Day 02 (2)" $ do
    it "Simple matrix" $
      D2.checksum2 (map (intersperse '\t') ["5928",
                                            "9473",
                                            "3865"])
      `shouldBe` 9
  -- describe "Day 03 (1)" $ do
  --   it "Square 1 takes 0 steps" $
  --     D3.distance 1 `shouldBe` 0
  --   it "Square 12 takes 3 steps" $
  --     D3.distance 12 `shouldBe` 3
  --   it "Square 23 takes 2 steps" $
  --     D3.distance 23 `shouldBe` 2
  --   it "Square 1024 takes 31 steps" $
  --     D3.distance 1024 `shouldBe` 31
  describe "Day 04 (1)" $ do
    it "aa bb cc dd ee is valid." $
      D4.hasNoDuplicates "aa bb cc dd ee" `shouldBe` True
    it "aa bb cc dd aa is not valid" $
      D4.hasNoDuplicates "aa bb cc dd aa" `shouldBe` False
    it "aa bb cc dd aaa is valid" $
      D4.hasNoDuplicates "aa bb cc dd aaa" `shouldBe` True
  describe "Day 04 (2)" $ do
    it "abcde fghij is a valid passphrase" $
      D4.hasNoAnagrams "abcde fghij" `shouldBe` True
    it "abcde xyz ecdab is not valid" $
      D4.hasNoAnagrams "abcde xyz ecdab" `shouldBe` False
    it "a ab abc abd abf abj" $
      D4.hasNoAnagrams "a ab abc abd abf abj" `shouldBe` True
    it "iiii oiii ooii oooi oooo is valid." $
      D4.hasNoAnagrams "iiii oiii ooii oooi oooo" `shouldBe` True
    it "oiii ioii iioi iiio is not valid" $
      D4.hasNoAnagrams "oiii ioii iioi iiio" `shouldBe` False
  -- describe "Day 05 (1)" $ do
  --   it "Simple maze should be escaped in five steps" $
  --     D5.getTerminationIndex [0, 3, 0, 1, -3] `shouldBe` 5
  describe "Day 05 (2)" $ do
    it "Simple maze should be escaped in ten steps" $
      D5.getTerminationIndex [0, 3, 0, 1, -3] `shouldBe` 10
  describe "Day 06 (1)" $ do
    it "Simple memory base loops after 5 reallocations" $
      D6.cyclesBeforeLoop [0, 2, 7, 0]  `shouldBe` 5
  describe "Day 07 (1)" $ do
    it "Small tree has tknk as its root" $
      D7.findRoot ["pbga (66)",
                   "xhth (57)",
                   "ebii (61)",
                   "havc (66)",
                   "ktlj (57)",
                   "fwft (72) -> ktlj, cntj, xhth",
                   "qoyq (66)",
                   "padx (45) -> pbga, havc, qoyq",
                   "tknk (41) -> ugml, padx, fwft",
                   "jptl (61)",
                   "ugml (68) -> gyxo, ebii, jptl",
                   "gyxo (61)",
                   "cntj (57)"]
      `shouldBe` "tknk"
  describe "Day 08 (1)" $ do
    it "Small program has highest register 1" $
      (show . D8.largestRegister . D8.run) ["b inc 5 if a > 1",
                                            "a inc 1 if b < 5",
                                            "c dec -10 if a >= 1",
                                            "c inc -20 if c == 10"]
      `shouldBe` "1"
