import Control.Concurrent

-- To compile:
-- # ghc -O3 -threaded factorize.hs

-- To run:
-- # ./factorize +RTS -N<num_threads>

factorize :: Int -> [Int]
factorize n = filter (\x -> n `mod` x == 0) [1..n]

-- For multi threading with 8 threads, we need to
-- split up the infinite list into 8 infinite lists...

-- For 2 threads, the infinite lists could be defined as follows:
-- [1,3..]
-- [2,4..]

-- For 3 threads, the following could work:
-- [1,4..]
-- [2,5..]
-- [3,6..]

g :: Int -> [[Int]]
g n =
  let l = [1..n*2] in
  map (\(a,b) -> [a,b..]) $ zip (take n l) (drop n l)

threadPrintFactorizeList :: Chan () -> [Int] -> IO ()
threadPrintFactorizeList endFlags l = do
  mapM_ (\n -> print $! factorize n) l
  writeChan endFlags ()

main :: IO ()
main = do
  endFlags <- newChan
  numThreads <- getNumCapabilities
  mapM_ (\l -> forkIO $ threadPrintFactorizeList endFlags l) (g numThreads)
  mapM_ (\_ -> readChan endFlags) [1..numThreads]
