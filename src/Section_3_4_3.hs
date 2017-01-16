module Section_3_4_3 where

import Numeric.LinearAlgebra

type Network = [(Matrix Double, Vector Double, Double -> Double)]

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

initNetwork :: Network
initNetwork =
  [ ( (2><3) [0.1, 0.3, 0.5, 0.2, 0.4, 0.6]
    , vector [0.1, 0.2, 0.3]
    , sigmoid
    )
  , ( (3><2) [0.1, 0.4, 0.2, 0.5, 0.3, 0.6]
    , vector [0.1, 0.2]
    , sigmoid
    )
  , ( (2><2) [0.1, 0.3, 0.2, 0.4]
    , vector [0.1, 0.2]
    , id
    )
  ]

forward :: Network -> Vector Double -> Vector Double
forward nw x = foldl (\x (w, b, h) -> cmap h (x <# w + b)) x nw
