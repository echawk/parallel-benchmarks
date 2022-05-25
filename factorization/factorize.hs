import Control.Concurrent

-- To compile:
-- # ghc -threaded factorize.hs

-- To run:
-- # ./factorize +RTS -N<num_threads>

factorize :: Int -> [Int]
factorize n = filter (\x -> n `mod` x == 0) [1..n]

print_factors :: Int -> IO ()
print_factors n =
  print $! factorize n

print_factorize_list :: [Int] -> IO ()
print_factorize_list l = do
  mapM_ print_factors l

-- For multi threading with 8 threads, we need to
-- split up the infinite list into 8 infinite lists...
--
-- For 2 threads, the infinite lists could be defined as follows:
-- [1,3..]
-- [2,4..]

-- For 3 threads, the following could work:
-- [1,4..]
-- [2,5..]
-- [3,6..]

h :: (Int, Int) -> [Int]
h (o,t) = [o,t..]

g :: Int -> [[Int]]
g n =
  let l = [1..n*2] in
  map h $ zip (take n l) (drop n l)

thread_print_factorize_list :: Chan () -> [Int] -> IO ()
thread_print_factorize_list endFlags l = do
  print_factorize_list l
  writeChan endFlags ()


main :: IO ()
main = do
  endFlags <- newChan
  numThreads <- getNumCapabilities
  mapM_ (\l -> forkIO $ thread_print_factorize_list endFlags l) (g numThreads)
  mapM_ (\_ -> readChan endFlags) [1..numThreads]
