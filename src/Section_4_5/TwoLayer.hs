{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Section_4_5.TwoLayer where

import Data.List

import Numeric.LinearAlgebra
import System.Random.MWC
import System.Random.MWC.Distributions (normal)

crossEntropyError :: Vector Double -> Vector Double -> Double
crossEntropyError y t =
  let delta = 1.0e-7
   in -1 * (sumElements $ t * cmap (\x -> log (x + delta)) y)

class Indexes c t where
  indexes :: c t -> [IndexOf c]

instance Container Vector t => Indexes Vector t where
  indexes v = [0 .. size v - 1]

instance (Num t, Container Vector t) => Indexes Matrix t where
  indexes m = let (x, y) = size m in (,) <$> [0..x-1] <*> [0..y-1]

numericalGradient :: (Container c Double, Additive (c Double), Indexes c Double) => (c Double -> Double) -> c Double -> c Double
numericalGradient f x =
  foldr add (assoc (size x) 0.0 []) $
    flip map (indexes x) $ \i ->
      let df1 = f (x `add` assoc (size x) 0.0 [(i,  h)])
          df2 = f (x `add` assoc (size x) 0.0 [(i, -h)])
          dfdx = (df1 - df2) / (2*h)
       in assoc (size x) 0.0 [(i, dfdx)]
    where h = 1.0e-4 -- 0.0001

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
initNetwork inputs as =
  withSystemRandom $ \gen ->
    sequence (zipWith (build gen) inouts as)
  where
    inouts :: [(Int, Int)]
    inouts = zip inputs (tail inputs)
    build :: GenIO -> (Int, Int) -> ActivationFunction -> IO Layer
    build gen (x, y) a = do
      w <- (x >< y) <$> (sequence $ replicate (x * y) (normal 0 0.01 gen))
      b <- vector <$> (sequence $ replicate y (normal 0 0.01 gen))
      pure (w, b, a)

-- | 認識（推論）を行う
predict :: Network -> Vector Double -> Vector Double
predict nw x = foldl (\x (w, b, h) -> h (x <# w + b)) x nw

-- | 損失関数の値を求める
loss :: [Vector Double] -> [Vector Double] -> Network -> Double
loss xs ts nw =
  let batchSize = genericLength xs
      ys = map (predict nw) xs
   in sum (zipWith crossEntropyError ys ts) / batchSize

-- | 認識精度を求める
accuracy :: [Vector Double] -> [Vector Double] -> Network -> Double
accuracy xs ts nw =
  let batchSize = genericLength xs
      ys = map (predict nw) xs
      yis = map maxIndex ys
      tis = map maxIndex ts
      correctCount = genericLength (filter id (zipWith (==) yis tis))
   in correctCount / batchSize

dismantle :: [a] -> [(a, a -> [a])]
dismantle xs = map (\i -> (xs!!i, (\x -> (take i xs) ++ [x] ++ (drop (i+1) xs)))) [0..length xs - 1] -- TODO more efficient implementation

updateNetwork :: ([Vector Double] -> [Vector Double] -> Network -> Double) -> [Vector Double] -> [Vector Double] -> Network -> Network
updateNetwork loss xs ts network = map (uncurry $ updateLayer xs ts) $ dismantle network
  where
    rate :: Double
    rate = 0.1
    updateLayer :: [Vector Double] -> [Vector Double] -> Layer -> (Layer -> Network) -> Layer
    updateLayer xs ts (w, b, a) mkNw =
      let dw = numericalGradient (\w -> loss xs ts (mkNw (w, b, a))) w
          db = numericalGradient (\b -> loss xs ts (mkNw (w, b, a))) b
       in (w - rate `scale` dw, b - rate `scale` db, a)
