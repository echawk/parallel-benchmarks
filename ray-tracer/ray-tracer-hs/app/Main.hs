module Main where

-- Tutorial: https://raytracing.github.io/books/RayTracingInOneWeekend.html

import Data.Word
import Data.ByteString (ByteString, pack)

import Graphics.Gloss

import Vec as V
import Ray as R

aspectRatio = 16.0 / 9.0

windowTitle = "Ray Tracer - Haskell"
windowWidth = 800 :: Int
windowHeight = 600 :: Int

imageWidth  = 400 :: Int
imageHeight = round ((fromIntegral imageWidth) / aspectRatio) :: Int

-- Camera
viewportHeight = 2.0
viewportWidth = aspectRatio * viewportHeight
focalLength = 1.0

origin = FloatVec3 0 0 0
horizontal = FloatVec3 viewportWidth 0 0
vertical = FloatVec3 0 viewportHeight 0
-- This is where it would be nice to have type classes...
lowerLeftCorner =  (floatVec3Minus
                    (floatVec3Minus
                     (floatVec3Minus origin (floatVec3DivN horizontal 2))
                     (floatVec3DivN vertical 2))
                     (FloatVec3 0 0 focalLength))

rayToColor :: Ray -> [Word8]
rayToColor (Ray o d) = r
  where
    ud = floatVec3Unit (d)
    t = 0.5 * ((floatVec3Y ud) + 1.0)
    ov = FloatVec3 1 1 1
    c = FloatVec3 0.5 0.7 1.0
    r = floatVec3ToColor $ floatVec3Plus (floatVec3MulN o (1 - t)) (floatVec3MulN c t)
        

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

-- FIXME: incorrect image on screen - correct is here:
-- https://raytracing.github.io/images/img-1.02-blue-to-white.png
coordToColor :: Int -> Int -> [Word8]
coordToColor width height =
    let fw  = fromIntegral width
        fh  = fromIntegral height
        fiw = fromIntegral imageWidth
        fih = fromIntegral imageHeight
        u = (fw / (fiw - 1))
        v = (fh / (fih - 1))
        uh = floatVec3MulN horizontal u
        vv = floatVec3MulN vertical v
        r = Ray (origin) (floatVec3Minus (floatVec3Plus (floatVec3Plus lowerLeftCorner uh) vv) origin)
    in rayToColor r

genCoords :: Int -> Int -> [(Int, Int)]
genCoords w h = concat $ map (\x -> map (\y -> (x, y)) [0..h-1]) [1..w-1]

main :: IO ()
main = display (InWindow windowTitle
               (windowWidth, windowHeight) (0, 0))
               white thePicture
