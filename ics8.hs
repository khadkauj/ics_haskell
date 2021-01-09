module Main where

fizzbuzz :: Integer -> String
fizzbuzz n |n `mod` 15 == 0 =  "fizzbuzz" |n `mod` 3 ==0 =  "fizz" |n `mod` 5 ==0 =  "buzz" | otherwise = show n

map' :: (a -> b) -> [a] -> [b]
map' fizzbuzz = foldr ((:) .fizzbuzz) []

map2 :: (a -> b) -> [a] -> [b]
map2 = foldl ((:).fizzbuzz) []





main = print()