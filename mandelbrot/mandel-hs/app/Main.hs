module Main where

-- https://mendo.zone/fun/beginners-haskell-bitmap-images/

import Data.Word
import Data.ByteString (ByteString, pack)

import Graphics.Gloss

windowWidth  = 800
windowHeight = 600
windowTitle  = "MandelBrot - Haskell"
numPixels    = windowWidth * windowHeight

mandelYMin   = -1.5
mandelYMax   =  1.5
mandelXMin   = -2.0
mandelXMax   =  2.0
mandelIters  =  80

bitmapData :: ByteString
bitmapData = pack $ concat $ map iterToColor [1..numPixels]

ourPicture :: Picture
ourPicture = bitmapOfByteString windowWidth windowHeight
             (BitmapFormat TopToBottom PxRGBA)
             bitmapData True

-- https://stackoverflow.com/questions/16500656/which-color-gradient-is-used-to-color-mandelbrot-in-wikipedia
iterToColor :: Int -> [Word8]
iterToColor numIters =
    let x = numIters `mod` 16 in
    case x of
      0  -> [ 66,  30,  15, 255]
      1  -> [ 25,   7,  26, 255]
      2  -> [  9,   1,  47, 255]
      3  -> [  4,   4,  73, 255]
      4  -> [  0,   7, 100, 255]
      5  -> [ 12,  44, 138, 255]
      6  -> [ 24,  82, 177, 255]
      7  -> [ 57, 125, 209, 255]
      8  -> [134, 181, 229, 255]
      9  -> [211, 236, 248, 255]
      10 -> [241, 233, 191, 255]
      11 -> [248, 201,  95, 255]
      12 -> [255, 170,   0, 255]
      13 -> [204, 128,   0, 255]
      14 -> [153,  87,   0, 255]
      15 -> [106,  52,   3, 255]

--
-- windowWidth  = 6
-- windowHeight = 2

-- = = = = = =
-- 1 2 3 4 5 6 |
-- 7 8 9 A B C |

--      x y
-- 1 -> 0,0
-- 2 -> 1,0
-- 3 -> 2,0
-- 4 -> 3,0
-- ...
-- 7 -> 0,1

-- Want a function that can convert a number
-- as shown above into coordinates.
-- FIXME
pixelNumToMandelCoord :: Int -> Int -> (Double, Double)
pixelNumToMandelCoord n height =
  let y = fromIntegral $ n `div` height
      x = fromIntegral $ n `mod` height
   in (x, y)

pixelNumToColor = 1 -- Actual mandelbrot alg.

main :: IO ()
main = display (InWindow windowTitle
                         (windowWidth, windowHeight) (0, 0))
                         white ourPicture
