module Section_4_5 where

import Data.List

import Numeric.LinearAlgebra
import System.Random.MWC
import System.Random.MWC.Distributions (normal)

crossEntropyError :: Vector Double -> Vector Double -> Double
crossEntropyError y t = -1 * (sumElements $ t * cmap log y)

numericalGradient :: Container c Double => (c Double -> Double) -> c Double -> c Double
numericalGradient f x =
  let h = 1e-4 -- 0.0001
      fxh1 = f $ cmap ((+) h) x
      fxh2 = f $ cmap ((-) h) x
   in (fxh1 - fxh2) / (2*h)

type ActivationFunction = Vector Double -> Vector Double

-- | シグモイド関数
sigmoid :: ActivationFunction
sigmoid = cmap (\x -> 1 / (1 + exp (-x)))

-- | ソフトマックス関数
softmax :: ActivationFunction
softmax a =
  let c = maxElement a
      exp_a = cmap (\x -> exp (x - c)) a
      sum_exp_a = sumElements exp_a
   in cmap (/sum_exp_a) exp_a

type Layer = ( Matrix Double      -- 重み行列
             , Vector Double      -- バイアス
             , ActivationFunction -- 活性化関数
             )
type Network = [Layer]

-- | 学習前のニューラルネットワークを生成する
initNetwork :: [Int]                -- 各層のユニット数
            -> [ActivationFunction] -- 各層の活性化関数
            -> IO Network
initNetwork inputs actfuncs =
  withSystemRandom $ \gen ->
    sequence (zipWith (build gen) inouts actfuncs)
  where
    inouts :: [(Int, Int)]
    inouts = zip inputs (tail inputs)
    build :: GenIO -> (Int, Int) -> ActivationFunction -> IO Layer
    build gen (x, y) actfunc = do
      w <- (x >< y) <$> (sequence $ replicate (x * y) (normal 0 0.01 gen))
      b <- vector <$> (sequence $ replicate y (normal 0 0.01 gen))
      pure (w, b, actfunc)

-- | 認識（推論）を行う
predict :: Network -> Vector Double -> Vector Double
predict nw x = foldl (\x (w, b, h) -> h (x <# w + b)) x nw

-- | 損失関数の値を求める
loss :: Network -> [Vector Double] -> [Vector Double] -> Double
loss nw xs ts =
  let batchSize = genericLength xs
      ys = map (predict nw) xs
   in sum (zipWith crossEntropyError ys ts) / batchSize

-- | 認識精度を求める
accuracy :: Network -> [Vector Double] -> [Vector Double] -> Double
accuracy nw xs ts =
  let batchSize = genericLength xs
      ys = map (predict nw) xs
      yis = map maxIndex ys
      tis = map maxIndex ts
      correctCount = genericLength (filter id (zipWith (==) yis tis))
   in correctCount / batchSize


main :: IO ()
main = do
  -- network <- initNetwork [784, 100, 10] [sigmoid, softmax]
  network <- initNetwork [5, 4, 3] [sigmoid, softmax]
  print $ predict network (vector [1,2,3,4,5])
