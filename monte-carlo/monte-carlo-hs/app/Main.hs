
module Main where

import System.Random

type Pair = (Double, Double)

monteCarlo :: [Pair] -> Int -> Double
monteCarlo tossesLst tossesNum =
    4 * fromIntegral numInCircle / (fromIntegral tossesNum)
    where numInCircle = length $ filter pairInCircle tossesLst

pairInCircle :: Pair -> Bool
pairInCircle (x, y) = x*x + y*y <= 1

main :: IO ()
main = do
  g <- newStdGen
  let totalTosses = 10000000
      lst = take totalTosses $ (randoms g :: [Pair]) in
    print $ monteCarlo lst totalTosses
