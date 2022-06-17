
module Main where

import System.Random
import Control.Parallel.Strategies
import Control.Concurrent

type Pair = (Double, Double)

monteCarlo :: [Pair] -> Int -> Double
monteCarlo tossesLst tossesNum =
    4 * fromIntegral numInCircle / (fromIntegral tossesNum)
    where numInCircle = length $ filter pairInCircle tossesLst

chunkList :: Int -> [a] -> [[a]]
chunkList chunkSize lst =
    let helper = (\cs l acc -> if cs >= (length l) then l : acc else helper cs (drop cs l) ((take cs l) : acc)) in
    helper chunkSize lst []

parMonteCarlo :: [Pair] -> Int -> Int -> Double
parMonteCarlo tossesLst tossesNum numCpus =
    let chunks = chunkList numCpus tossesLst
        numInCircle = length $ concat (map (\l -> filter pairInCircle l) chunks `using` parList rseq)
    in 4 * fromIntegral numInCircle / (fromIntegral tossesNum)

pairInCircle :: Pair -> Bool
pairInCircle (x, y) = x*x + y*y <= 1

main :: IO ()
main = do
  g <- newStdGen
  numCpu <- getNumCapabilities
  let totalTosses = 1000
      lst = take totalTosses $ (randoms g :: [Pair]) in
      do
          print $ monteCarlo    lst totalTosses
          print $ parMonteCarlo lst totalTosses numCpu
