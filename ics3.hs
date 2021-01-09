module Main where

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = take (length xs) (drop n (cycle xs))

circle :: [a] -> [[a]]
circle [] = [[]]
circle xs = [ rotate x xs | x <- [0..(length(xs))-1] ]

main = do
    print()