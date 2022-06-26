module Main where

-- Tutorial: https://raytracing.github.io/books/RayTracingInOneWeekend.html

import Data.Word
import Data.ByteString (ByteString, pack)

import Graphics.Gloss

import Vec as V

windowTitle = "Ray Tracer - Haskell"
windowWidth = 800 :: Int
windowHeight = 600 :: Int

imageWidth  = 256 :: Int
imageHeight = 256 :: Int

-- FIXME: I think the image is being flipped?
imageData :: ByteString
imageData = pack
  $ concat
  $ map (\(x, y) -> coordToColor x y)
  $ reverse
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
        r = (fw / fiw)
        g = (fh / fih)
        b = (0.25)
    in floatVec3ToColor (FloatVec3 r g b)

genCoords :: Int -> Int -> [(Int, Int)]
genCoords w h = concat $ map (\x -> map (\y -> (x, y)) [0..h-1]) [1..w-1]

main :: IO ()
main = display (InWindow windowTitle
               (windowWidth, windowHeight) (0, 0))
               white thePicture
