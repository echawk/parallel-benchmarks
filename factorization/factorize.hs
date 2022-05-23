
factorize :: Int -> [Int]
factorize n = filter (\x -> n `mod` x == 0) [1..n]

main = do
    print $ map factorize [1..]

