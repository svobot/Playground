module Questions3 where

import           System.Random                  ( randomRIO )

-- Problem 21
--- Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n =
    let (before, after) = splitAt (n - 1) xs in before ++ x : after

insertAt' :: a -> [a] -> Int -> [a]
insertAt' y xs 1 = y : xs
insertAt' y [] n | n == 1    = [y]
                 | otherwise = error "insertAt': index out of range"
insertAt' y (x : xs) n = x : insertAt' y xs (n - 1)

-- Problem 22
--- Create a list containing all integers within a given range.
range :: Int -> Int -> [Int]
range x y = [x .. y]

range' :: Int -> Int -> [Int]
range' x y | x < y     = x : range' (x + 1) y
           | x == y    = [x]
           | otherwise = reverse $ range' y x

-- Problem 23
--- Extract a given number of randomly selected elements from a list.

-- rnd_select :: [a] -> Int -> IO [a]
-- TODO: implement

-- Problem 24
--- Lotto: Draw N different random numbers from the set 1..M.

-- TODO: finish exercises
