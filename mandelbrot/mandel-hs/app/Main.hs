module Main where

-- https://mendo.zone/fun/beginners-haskell-bitmap-images/

import Data.Word
import Data.ByteString (ByteString, pack)

import Graphics.Gloss

windowWidth  = 8 --00
windowHeight = 6 --00
windowTitle  = "MandelBrot - Haskell"
numPixels    = windowWidth * windowHeight

mandelYMin   = -1.5
mandelYMax   =  1.5
mandelXMin   = -2.0
mandelXMax   =  2.0
mandelIters  =  80

bitmapData :: ByteString
bitmapData = pack $ concat $ map iterToColor [0..numPixels-1]

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

-- Want a function that can convert a number
-- as shown above into coordinates.
-- FIXME
pixelNumToPixelCoord :: Int -> (Int, Int)
pixelNumToPixelCoord n =
    let y = n `div` windowWidth
        x = n - (n `div` windowWidth * windowWidth)
    in (x, y)

pixelCoordToMandelCoord :: (Int, Int) -> (Double, Double)
pixelCoordToMandelCoord (x, y) =
    let mx = (x / (windowWidth  - 1)) * (mandelXMax - mandelXMin) + mandelXMin
        my = (y / (windowHeight - 1)) * (mandelYMax - mandelYMin) + mandelYMin
    in (mx, my)

pixelNumToColor = 1 -- Actual mandelbrot alg.

main :: IO ()
main = display (InWindow windowTitle
                         (windowWidth, windowHeight) (0, 0))
                         white ourPicture
