{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Map          (fromList)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Data.List         (intersperse)

import Day01 as D1 (checksum, checksum2)
import Day02 as D2 (checksum, checksum2)

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
