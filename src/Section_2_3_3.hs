module Section_2_3_3 where

import Numeric.LinearAlgebra

and' :: Double -> Double -> Double
and' x1 x2 =
  let x = vector [x1, x2]
      w = vector [0.5, 0.5]
      b = (-0.7)
      tmp = w <.> x + b
   in if tmp <= 0
         then 0
         else 1

nand :: Double -> Double -> Double
nand x1 x2 =
  let x = vector [x1, x2]
      w = vector [(-0.5), (-0.5)]
      b = 0.7
      tmp = w <.> x + b
   in if tmp <= 0
         then 0
         else 1

or' :: Double -> Double -> Double
or' x1 x2 =
  let x = vector [x1, x2]
      w = vector [0.5, 0.5]
      b = (-0.2)
      tmp = w <.> x + b
   in if tmp <= 0
         then 0
         else 1

