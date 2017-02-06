{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module MNIST where

import Codec.Compression.GZip (decompress)
import Control.Monad (when)
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)
import Network.HTTP.Simple (Request, httpLBS, getResponseBody)
import System.Directory (doesFileExist)

import Data.Proxy

class HasImageParser image where
  type Image image
  imageParser :: Proxy image -> Int -> Int -> Parser (Image image)
  normalize :: Proxy image -> Image image -> Image image

data MatrixImage

instance HasImageParser MatrixImage where
  type Image MatrixImage = [[Double]]
  imageParser _ w h = P.count h (P.count w (fromIntegral <$> P.anyWord8))
  normalize _ xs = map (map (/ (sum (map sum xs)))) xs

data FlattenImage

instance HasImageParser FlattenImage where
  type Image FlattenImage = [Double]
  imageParser _ w h = P.count (w * h) (fromIntegral <$> P.anyWord8)
  normalize _ xs = map (/ (sum xs)) xs

class HasLabelParser label where
  type Label label
  labelParser :: Proxy label -> Parser (Label label)

data NumberLabel

instance HasLabelParser NumberLabel where
  type Label NumberLabel = Word8
  labelParser _ = P.anyWord8

data OneHotLabel

instance HasLabelParser OneHotLabel where
  type Label OneHotLabel = [Word8]
  labelParser _ = fmap (\w -> map (\v -> if w == v then 1 else 0) [0..9]) P.anyWord8

data Labels label = Labels Int [label] deriving Show
data Images image = Images Int Int Int [image] deriving Show

words2Int :: [Word8] -> Int
words2Int xs = sum $ zipWith (*) (reverse xs') (map (256^) [0..])
  where xs' = map fromIntegral xs

labelsParser :: HasLabelParser label => Proxy label -> Parser (Labels (Label label))
labelsParser proxy = do
  P.string $ BS.pack [0,0,8,1]
  len <- words2Int <$> P.count 4 P.anyWord8
  labels <- P.count len (labelParser proxy)
  pure (Labels len labels)

imagesParser :: HasImageParser image => Proxy image -> Parser (Images (Image image))
imagesParser proxy = do
  P.string $ BS.pack [0,0,8,3]
  len    <- words2Int <$> P.count 4 P.anyWord8
  width  <- words2Int <$> P.count 4 P.anyWord8
  height <- words2Int <$> P.count 4 P.anyWord8
  images <- P.count len (imageParser proxy width height)
  pure (Images len width height images)

data Training
data Test

class HasLabelFileLocate a where
  labelFilePath :: Proxy a -> String
  labelFileRequest :: Proxy a -> Request

instance HasLabelFileLocate Test where
  labelFilePath _ = "data/t10k-labels-idx1-ubyte.gz"
  labelFileRequest _ = "http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz"

instance HasLabelFileLocate Training where
  labelFilePath _ = "data/train-labels-idx1-ubyte.gz"
  labelFileRequest _ = "http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz"

class HasImageFileLocate a where
  imageFilePath :: Proxy a -> String
  imageFileRequest :: Proxy a -> Request

instance HasImageFileLocate Test where
  imageFilePath _ = "data/t10k-images-idx3-ubyte.gz"
  imageFileRequest _ = "http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz"

instance HasImageFileLocate Training where
  imageFilePath _ = "data/train-images-idx3-ubyte.gz"
  imageFileRequest _ = "http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz"

readOrDownloadFile :: FilePath -> Request -> IO BS.ByteString
readOrDownloadFile path req = do
  doesExist <- doesFileExist path
  when (not doesExist) downloadFile
  BL.toStrict . decompress <$> BL.readFile path
   where
    downloadFile :: IO ()
    downloadFile = do
      BS.writeFile path =<< (BL.toStrict . getResponseBody <$> httpLBS req)

getLabels :: (HasLabelFileLocate phase, HasLabelParser label) => Proxy phase -> Proxy label -> IO (Labels (Label label))
getLabels phaseProxy labelProxy = do
  content <- readOrDownloadFile (labelFilePath phaseProxy) (labelFileRequest phaseProxy)
  case P.parseOnly (labelsParser labelProxy) content of
    Left msg -> error msg
    Right labels -> pure labels

getImages :: (HasImageFileLocate phase, HasImageParser image) => Bool -> Proxy phase -> Proxy image -> IO (Images (Image image))
getImages doNormalize phaseProxy imageProxy = do
  content <- readOrDownloadFile (imageFilePath phaseProxy) (imageFileRequest phaseProxy)
  case P.parseOnly (imagesParser imageProxy) content of
    Left msg -> error msg
    Right (Images len width height images) -> pure (Images len width height (if doNormalize then map (normalize imageProxy) images else images))

displayImage :: [[Double]] -> IO ()
displayImage image = mapM_ putStrLn (map (map toGrayscale) image)
  where
    grayscale = " .:-=+*#%@"
    index x = (10 * ceiling x) `quot` 256
    toGrayscale x = grayscale !! (index x)

main :: IO ()
main = do
  (Images _ _ _ images) <- getImages False (Proxy @Test) (Proxy @MatrixImage)
  displayImage (images !! 1)
