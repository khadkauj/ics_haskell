module Main where

count :: Integer -> Integer
count 0 = 0
count n = n `mod` 2 + count(n `div` 2)

isEvil :: Integer -> Bool
isEvil n |count n  `mod` 2 == 0 = True |otherwise = False


isOdious :: Integer -> Bool
isOdious n |count n`mod`2 ==0 =  False |otherwise = True

evils :: [Integer] 
evils = [x | x <- [0..], isEvil x == True]

odious :: [Integer]
odious  = [x | x <- [0..], isOdious x == True]

main = print()

