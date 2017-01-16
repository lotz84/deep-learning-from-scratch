module Spec_2_5_2 where

import Test.Hspec
import Section_2_5_2 (xor)

spec :: Spec
spec = do
  describe "xor" $ do
    it "xor 0 0 == 0" $ do
      xor 0 0 `shouldBe` 0
    it "xor 1 0 == 0" $ do
      xor 1 0 `shouldBe` 1
    it "xor 0 1 == 0" $ do
      xor 0 1 `shouldBe` 1
    it "xor 1 1 == 1" $ do
      xor 1 1 `shouldBe` 0
