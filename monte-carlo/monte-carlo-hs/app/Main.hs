
module Main where

import System.Random
import Control.Parallel.Strategies
import Control.Concurrent

type Pair = (Double, Double)

monteCarlo :: [Pair] -> Int -> Double
monteCarlo tossesLst tossesNum =
    4 * fromIntegral numInCircle / (fromIntegral tossesNum)
    where numInCircle = length $ filter pairInCircle tossesLst

parMonteCarlo :: [Pair] -> Int -> Int -> Double
parMonteCarlo tossesLst tossesNum numCpus =
    let chunkSize = length tossesLst `div` numCpus
        numInCircle = length (filter pairInCircle tossesLst `using` parListChunk chunkSize rseq)
    in 4 * fromIntegral numInCircle / (fromIntegral tossesNum)

pairInCircle :: Pair -> Bool
pairInCircle (x, y) = x*x + y*y <= 1

main :: IO ()
main = do
  g <- newStdGen
  numCpu <- getNumCapabilities
  let totalTosses = 10000000
      lst = take totalTosses $ (randoms g :: [Pair]) in
      do
          print $ monteCarlo    lst totalTosses
          print $ parMonteCarlo lst totalTosses numCpu
