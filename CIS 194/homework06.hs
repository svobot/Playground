{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
        show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x $ interleaveStreams ys xs

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) ((streamMap (+ 1) ruler))

x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
        fromInteger n = Cons n $ streamRepeat 0
        negate (Cons a as) = Cons (negate a) $ negate as
        (Cons a as) + (Cons b bs) = Cons (a + b) $ as + bs
        (Cons a0 as) * b@(Cons b0 bs) =
                Cons (a0 * b0) $ fromInteger a0 * bs + as * b

instance Fractional (Stream Integer) where
        (Cons a0 as) / (Cons b0 bs) = q
                where q = Cons (a0 `div` b0) $ (as - q * bs) / fromInteger b0

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ 2)

data Matrix = Matrix Integer Integer Integer Integer deriving Show

instance Num Matrix where
        (Matrix a1 b1 c1 d1) * (Matrix a2 b2 c2 d2) = Matrix
                (a1 * a2 + b1 * c2)
                (a1 * b2 + b1 * d2)
                (c1 * a2 + d1 * c2)
                (c1 * b2 + d1 * d2)

fibs4 :: Integer -> Integer
fibs4 0 = 0
fibs4 n = let Matrix _ x _ _ = Matrix 1 1 1 0 ^ n in x

