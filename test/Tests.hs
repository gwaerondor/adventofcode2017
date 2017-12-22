{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Map          (fromList)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Day01 (checksum)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "Advent of Code 2017" $
  describe "Day 01 (1)" $ do
    it "Two separate matches" $
      checksum "1122" `shouldBe` 3
    it "Four numbers matching each other" $
      checksum "1111" `shouldBe` 4
    it "No matches" $
      checksum "1234" `shouldBe` 0
    it "Cyclical property" $
      checksum "91212129" `shouldBe` 9
