module Spec_2_3_1 where

import Test.Hspec
import Section_2_3_1 (and')

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
