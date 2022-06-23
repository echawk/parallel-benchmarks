module Main where

-- Tutorial: https://raytracing.github.io/books/RayTracingInOneWeekend.html

import Data.Word
import Data.ByteString (ByteString, pack)

import Graphics.Gloss

windowTitle = "Ray Tracer - Haskell"
windowWidth = 800 :: Int
windowHeight = 600 :: Int

imageWidth  = 256 :: Int
imageHeight = 256 :: Int

imageData :: ByteString
imageData = pack $ concat
  $ map (\(x, y) -> coordToColor x y)
  $ genCoords imageWidth imageHeight

thePicture :: Picture
thePicture = bitmapOfByteString imageWidth imageHeight
             (BitmapFormat TopToBottom PxRGBA)
             imageData True

coordToColor :: Int -> Int -> [Word8]
coordToColor width height =
    let fw  = fromIntegral width 
        fh  = fromIntegral height  
        fiw = fromIntegral imageWidth 
        fih = fromIntegral imageHeight
        r = (fh / fih) * 255.999
        g = (fw / fiw) * 255.999 
        b = (0.25)     * 255.999 
        ir = fromInteger (round r) :: Word8
        ig = fromInteger (round g) :: Word8
        ib = fromInteger (round b) :: Word8
    in [ir, ig, ib, 255]

genCoords :: Int -> Int -> [(Int, Int)]
genCoords w h = concat $ map (\x -> map (\y -> (x, y)) [0..h-1]) [1..w-1]

main :: IO ()
main = display (InWindow windowTitle
               (windowWidth, windowHeight) (0, 0))
               white thePicture
