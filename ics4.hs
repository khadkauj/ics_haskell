module Main where
isPrime :: Integer -> Bool
isPrime n = null [ x | x <- [2..n  `div`  2], n `mod` x == 0 ]

primes :: Integer -> Integer -> [Integer]
primes a b = [ x | x <- [a..b], isPrime x]

gappies :: Integer -> Integer -> Integer -> [(Integer,Integer)]
gappies g a b = [(x,y) | x <- primes a b ,  y <- primes a b , y-x==g , x>1]


twins = gappies 2
cousins = gappies 4
sexies = gappies 6