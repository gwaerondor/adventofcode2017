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
import Day09 as D9 (score, countGarbage)
import Day10 as D10 (hash, hash2, denseHash, toHex)
import Day12 as D12 (countSizeOfGroupZero, countNumberOfGroups)
import Day13 as D13 (countSeverity, countTicks)
import Day15 as D15 (bitsMatch, nextValueA, nextValueB, nextValueA2, nextValueB2, judge, judge2)
import Day16 as D16 (dance, parseInstruction, run', Instruction(..))
import Day17 as D17 (spinlock)
import Day18 as D18 (firstRecoveredSound)

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
      (show . D8.largestRegister . fst . D8.run) ["b inc 5 if a > 1",
                                                 "a inc 1 if b < 5",
                                                 "c dec -10 if a >= 1",
                                                 "c inc -20 if c == 10"]
      `shouldBe` "1"
  describe "Day 08 (2)" $ do
    it "Small program has historically highest register 10" $
      (show . snd . D8.run) ["b inc 5 if a > 1",
                              "a inc 1 if b < 5",
                              "c dec -10 if a >= 1",
                              "c inc -20 if c == 10"]
      `shouldBe` "10"
  describe "Day 09 (1)" $ do
    it "Tiny group has score of 1" $
      D9.score "{}" `shouldBe` 1
    it "Nested groups has higher score" $
      D9.score "{{{}}}" `shouldBe` 6
    it "Multiple nested groups have additive score" $
      D9.score "{{},{}}" `shouldBe` 5
    it "Multiple deeply nested groups" $
      D9.score "{{{},{},{{}}}}" `shouldBe` 16
    it "Garbage should be ignored" $
      D9.score "{<a>,<a>,<a>,<a>}" `shouldBe` 1
    it "Ignore nested garbage" $
      D9.score "{{<ab>},{<ab>},{<ab>},{<ab>}}" `shouldBe` 9
    it "Bang should invalidate next char even if it is a bang" $
      D9.score "{{<!!>},{<!!>},{<!!>},{<!!>}}" `shouldBe` 9
    it "Bang should invalidate end of garbage symbol" $
      D9.score "{{<a!>},{<a!>},{<a!>},{<ab>}}" `shouldBe` 3
  describe "Day 09 (2)" $ do
    it "Empty garbage has length 0" $
      D9.countGarbage "<>" `shouldBe` 0
    it "Some regular garbage has length 17" $
      D9.countGarbage "<random characters>" `shouldBe` 17
    it "Garbage can contain <" $
      D9.countGarbage "<<<<>" `shouldBe` 3
    it "Cancelled garbage doesn't count" $
      D9.countGarbage "<{!>}>" `shouldBe` 2
    it "Bang cancels bang" $
      D9.countGarbage "<!!>" `shouldBe` 0
    it "Bang cancels >" $
      D9.countGarbage "<!!!>>" `shouldBe` 0
    it "Some of each has 10 garbage chars" $
      D9.countGarbage "<{o\"i!a,<{i<a>" `shouldBe` 10
  describe "Day 10 (1)" $ do
    it "A short list with a short input" $
      D10.hash [0..4] [3, 4, 1, 5] `shouldBe` [3, 4, 2, 1, 0]
  describe "Day 10 (2)" $ do
    it "A dense hash of a single block (16 bytes)" $
      D10.denseHash [65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22] `shouldBe` [64]
    it "The hex representation of 64 is 40" $
      D10.toHex [64] `shouldBe` "40"
    it "Hash of the empty string" $
      (D10.toHex $ D10.hash2 [0..255] []) `shouldBe` "a2582a3a0e66e6e86e3812dcb672a272"
  describe "Day 12 (1)" $ do
    it "Small sample with 6 programs in group 0" $
      D12.countSizeOfGroupZero ["0 <-> 2",
                                "1 <-> 1",
                                "2 <-> 0, 3, 4",
                                "3 <-> 2, 4",
                                "4 <-> 2, 3, 6",
                                "5 <-> 6",
                                "6 <-> 4, 5"]
      `shouldBe` 6
  describe "Day 12 (2)" $ do
    it "Small sample with two groups" $
      D12.countNumberOfGroups ["0 <-> 2",
                               "1 <-> 1",
                               "2 <-> 0, 3, 4",
                               "3 <-> 2, 4",
                               "4 <-> 2, 3, 6",
                               "5 <-> 6",
                               "6 <-> 4, 5"]
      `shouldBe` 2
  describe "Day 13 (1)" $ do
    it "Small firewall has severity of 24" $
      D13.countSeverity ["0: 3", "1: 2", "4: 4", "6: 4"] `shouldBe` 24
  describe "Day 13 (2)" $ do
    it "Small firewall must wait for 10 ticks" $
      D13.countTicks ["0: 3", "1: 2", "4: 4", "6: 4"] `shouldBe` 10
  describe "Day 15 (1)" $ do
    it "The 16 least significant bits match" $
      D15.bitsMatch 245556042 1431495498 `shouldBe` True
    it "The 16 least significant bits do not match" $
      D15.bitsMatch 1092455 430625591 `shouldBe` False
    it "Generating the next value for generator A" $
      (take 6 $ iterate D15.nextValueA 65)
      `shouldBe` [65, 1092455, 1181022009, 245556042, 1744312007, 1352636452]
    it "Generating the next value for generator B" $
      (take 6 $ iterate D15.nextValueB 8921)
      `shouldBe` [8921, 430625591, 1233683848, 1431495498, 137874439, 285222916]
    it "First five iterations have a judgment of 1" $
      D15.judge 65 8921 0 5 `shouldBe` 1
  describe "Day 15 (2)" $ do
    it "Values by generator A must be divisible by 4" $
      (take 6 $ iterate D15.nextValueA2 65)
      `shouldBe` [65, 1352636452, 1992081072, 530830436, 1980017072, 740335192]
    it "Values by generator B must be divisible by 8" $
      (take 6 $ iterate D15.nextValueB2 8921)
      `shouldBe` [8921, 1233683848, 862516352, 1159784568, 1616057672, 412269392]
    it "Five iterations have a judgment of 0" $
      D15.judge2 65 8921 0 5 `shouldBe` 0
  describe "Day 16 (1)" $ do
    it "Five programs with three instructions" $
      D16.dance "abcde" "s1,x3/4,pe/b" `shouldBe` "baedc"
  describe "Day 16 (2)" $ do
    it "Five programs with three instructions, twice" $
      D16.run' 2 [Spin 1,
                  Exchange 3 4,
                  Partner 'e' 'b'] "abcde"
      `shouldBe` "ceadb"
  describe "Day 17 (1)" $ do
    it "Three steps per insert gives 638 after final insertion" $
      D17.spinlock 3 0 2017 [0] `shouldBe` [2017, 638]
  describe "Day 18 (1)" $ do
    it "Short sound file plays a frequency of 4" $
      D18.firstRecoveredSound ["set a 1",
                               "add a 2",
                               "mul a a",
                               "mod a 5",
                               "snd a",
                               "set a 0",
                               "rcv a",
                               "jgz a -1",
                               "set a 1",
                               "jgz a -2"]
      `shouldBe` 4
