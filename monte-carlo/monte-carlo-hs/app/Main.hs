
module Main where

import System.Random

type Pair = (Double, Double)    

monteCarlo :: [Pair] -> Double
monteCarlo tossesLst =
    4 * fromIntegral numInCircle / (fromIntegral (length tossesLst))
    where numInCircle = length $ filter pairInCircle tossesLst 
 
pairInCircle :: Pair -> Bool
pairInCircle (x, y) = x*x + y*y <= 1

main :: IO ()
main = do
  g <- newStdGen 
  let totalTosses = 100000
      lst = take totalTosses $ (randoms g :: [Pair]) in
    print $ monteCarlo lst 
