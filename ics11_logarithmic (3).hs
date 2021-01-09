module Main where

pow'' :: Integer -> Integer -> Integer
pow'' x n = go x n
    where
        go x n
            | n == 0 = 1
            | n == 1 = x
            | n `mod` 2 == 0 = (go x (n `div` 2))  * (go x (n `div` 2))
            |otherwise = x * (go x (n - 1)) 

main = do
    print(pow'' 2 6)



