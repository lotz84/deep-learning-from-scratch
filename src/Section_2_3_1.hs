module Section_2_3_1 where

and' :: Double -> Double -> Double
and' x1 x2 =
  let (w1, w2, theta) = (0.5, 0.5, 0.7)
      tmp = x1*w1 + x2*w2
   in if tmp <= theta
         then 0
         else 1
