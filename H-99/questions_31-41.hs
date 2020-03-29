module Questions4 where

-- Problem 31
--- Determine whether a given integer number is prime.
isPrime :: Integer -> Bool
isPrime n
    | n < 2     = False
    | otherwise = foldr (\x acc -> n `mod` x /= 0 && acc) True [2 .. n - 1]

-- Problem 32
--- Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
myGCD :: Int -> Int -> Int
myGCD n 0 = abs n
myGCD n m = myGCD m (n `mod` m)

-- Problem 33
--- Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.
coprime :: Int -> Int -> Bool
coprime n m = myGCD m n == 1

t = coprime 35 64 -- True
