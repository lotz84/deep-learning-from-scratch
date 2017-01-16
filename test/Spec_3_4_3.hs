module Spec_3_4_3 where

import Test.Hspec
import Numeric.LinearAlgebra

import Section_3_4_3 (initNetwork, forward)

spec :: Spec
spec = do
  describe "forward" $ do
    it "(1.0, 0.5) -> (0.326, 0.696)" $ do
        let x = vector [1.0, 0.5]
            answer = vector [0.31682708, 0.69627909]
            prediction = forward initNetwork x
            err = 0.0001
        norm_2 (answer - prediction) < err `shouldBe` True
