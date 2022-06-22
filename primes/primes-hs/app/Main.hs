module Main where

import Control.Parallel.Strategies

isPrime :: Int -> Bool
isPrime n = foldl (&&) True $ map (\y -> n `rem` y /= 0) [2..floor(sqrt(fromIntegral n))]

-- Infinite list of all of the primes. Uses the sieve of Eratosthenes in Parallel.
primes :: [Int]
primes = filter isPrime [2..] --`using` parListChunk 500 rseq

main :: IO ()
main = do
  putStrLn $ show $ take 100 primes
