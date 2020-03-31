module QuadraticSieve where

import           Control.Monad
import           Data.Maybe
import qualified Data.IntSet                   as IntSet
import           Data.List
import           Data.Function

import           Math.NumberTheory.Primes
import           Math.NumberTheory.Moduli.Jacobi
import           Math.NumberTheory.SmoothNumbers

-- Smoothness bound for @n@, computed as @exp(0.5 * sqrt(ln(n) * ln(ln(n))))@.
smoothnessBound :: Integer -> Integer
smoothnessBound =
  ceiling . exp . (* 0.5) . sqrt . liftM2 (*) log (log . log) . fromInteger

-- Primes smaller than the smoothness bound for @n@ and for which @n@ is a quadratic residue.
factorBase :: Integer -> [Integer]
factorBase n =
  takeWhile (<= smoothnessBound n)
    $ unPrime (nextPrime 2)
    : [ prime | prime <- unPrime <$> [nextPrime 3 ..], jacobi n prime == One ]

type Relation = (Integer, Integer)

-- Find smooth relations.
--
-- In a smooth relation @(x, x^2 - n)@, @x^2 - n@ is smooth over a given base.
smoothRelations :: [Integer] -> Integer -> [Relation]
smoothRelations base n =
  let start       = ceiling . sqrt $ fromInteger n
      smoothBasis = fromJust $ fromList base
      size        = 1 + length base
  in  take
        size
        [ (x, y) | x <- [start ..], let y = x ^ 2 - n, isSmooth smoothBasis y ]

-- Find maximal @p@ such that @prime ^ p@ divides @n@
factorPower :: Integer -> Integer -> Integer
factorPower prime n =
  if n `mod` prime == 0 then 1 + factorPower prime (n `div` prime) else 0

-- Give the exponent vector of @n@.
--
-- Exponent vector of @n@, over a given base @base@, 
-- contains the powers of the primes in @base@ that factor @n@.
--
-- >>> exponentVector [2,3,5,7] 40
-- [3,0,1,0]
exponentVector :: [Integer] -> Integer -> [Integer]
exponentVector base n = snd $ foldr
  (\prime (rem, vector) ->
    let power = factorPower prime rem
    in  (rem `div` (prime ^ power), power : vector)
  )
  (n, [])
  base

-- Find coefficients of a linear combination of @rows@.
combination :: [[Integer]] -> IntSet.IntSet
combination rows =
  let coeffs = zip (map IntSet.singleton [0 ..]) rows
      combineVecs
        :: (IntSet.IntSet, [Integer])
        -> (IntSet.IntSet, [Integer])
        -> (IntSet.IntSet, [Integer])
      combineVecs (s1, x) (s2, y) =
          (IntSet.union s1 s2, map (`mod` 2) $ zipWith (+) x y)
      sortCombinations = sortBy (compare `on` snd)
  in  fst . head . sortCombinations $ foldr
        (\x cs -> x : [ combineVecs x y | y <- cs ] ++ cs)
        []
        coeffs

-- Take only the elements of @xs@ specified by the indexes in @indexes@.
filterByIndexes :: IntSet.IntSet -> [a] -> [a]
filterByIndexes indexes xs =
  reverse
    . fst
    . foldl'
        (\(result, rest) i ->
          let rest' = drop i rest in (head rest' : result, rest')
        )
        ([], xs)

    . diff
    . IntSet.toAscList
    $ indexes
 where
  diff []         = []
  diff is@(i : _) = i : zipWith (-) (drop 1 is) is

-- Factor a given composite number @n@ using the quadratic sieve.
factor n =
  let base            = factorBase n
      smoothRels      = smoothRelations base n
      exponentVecs    = map (exponentVector base . snd) smoothRels
      dependentSubset = (filterByIndexes =<< combination) exponentVecs
      primePowers     = foldr (zipWith (+)) [0 ..] dependentSubset
      y = foldr (\(prime, exp) acc -> (acc * prime ^ exp) `mod` n) 1
        $ zip base primePowers
      x = product . map fst $ smoothRels
  in  gcd (x - y) n
