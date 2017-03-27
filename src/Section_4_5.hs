{-# LANGUAGE TypeApplications #-}

module Section_4_5 where

import MNIST
import Numeric.LinearAlgebra
import Section_4_5.TwoLayer

main :: IO ()
main = do
  putStrLn "Loading Labels..."
  (Labels _ labels)     <- getLabels @Training @OneHotLabel

  putStrLn "Loading Images..."
  (Images _ _ _ images) <- getImages @Training @FlattenImage True

  putStrLn "Creating Initial Network..."
  network <- initNetwork [784, 50, 10] [sigmoid, softmax]

  putStrLn "Picking Learning Data..."
  let label = vector $ map fromIntegral $ labels !! 10
      image = vector $ images !! 10

  putStrLn $ "Answer: " ++ show label

  putStrLn $ "Prediction before learning: " ++ show (predict network image)

  putStrLn "Learning Network..."
  let nw = updateNetwork loss [image] [label] network

  putStrLn $ "Prediction after learning: " ++ show (predict nw image)
