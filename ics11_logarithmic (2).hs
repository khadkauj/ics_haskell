module Main where

pow' :: Integer -> Integer -> Integer

pow' x n = go x n 1
    where
        go x n k
            | n == 0 = k
            | n `mod` 2 == 0 = go (x*x) (n`div`2) (k)
            | otherwise = go (x ) (n - 1) (x  * k)

main = do
    print(pow' 2 5)