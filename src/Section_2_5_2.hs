module Section_2_5_2 where

import Numeric.LinearAlgebra

import Section_2_3_3 (and', nand, or')

xor :: Double -> Double -> Double
xor x1 x2 =
  let s1 = nand x1 x2
      s2 = or' x1 x2
      y = and' s1 s2
   in y
