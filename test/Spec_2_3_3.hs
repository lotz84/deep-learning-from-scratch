module Spec_2_3_3 where

import Test.Hspec
import Section_2_3_3 (and', nand, or')

spec :: Spec
spec = do
  describe "and'" $ do
    it "and' 0 0 == 0" $ do
      and' 0 0 `shouldBe` 0
    it "and' 1 0 == 0" $ do
      and' 1 0 `shouldBe` 0
    it "and' 0 1 == 0" $ do
      and' 0 1 `shouldBe` 0
    it "and' 1 1 == 1" $ do
      and' 1 1 `shouldBe` 1
  describe "nand" $ do
    it "nand 0 0 == 0" $ do
      nand 0 0 `shouldBe` 1
    it "nand 1 0 == 0" $ do
      nand 1 0 `shouldBe` 1
    it "nand 0 1 == 0" $ do
      nand 0 1 `shouldBe` 1
    it "nand 1 1 == 1" $ do
      nand 1 1 `shouldBe` 0
  describe "or'" $ do
    it "or' 0 0 == 0" $ do
      or' 0 0 `shouldBe` 0
    it "or' 1 0 == 0" $ do
      or' 1 0 `shouldBe` 1
    it "or' 0 1 == 0" $ do
      or' 0 1 `shouldBe` 1
    it "or' 1 1 == 1" $ do
      or' 1 1 `shouldBe` 1
