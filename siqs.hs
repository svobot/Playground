module QuadraticSieve where

import           Control.Monad
import           Data.Maybe
import           Data.IntMap.Strict             ( IntMap )
import qualified Data.IntMap.Strict            as IntMap
import qualified Data.IntSet                   as IntSet
import           Data.Bits
import           Data.Int
import           Data.List
import           Data.Function

import           Math.NumberTheory.Primes       ( Prime
                                                , nextPrime
                                                , unPrime
                                                )
import qualified Math.NumberTheory.Moduli.Class
                                               as ModClass
import           Math.NumberTheory.Moduli.Jacobi
import           Math.NumberTheory.SmoothNumbers
import           Math.NumberTheory.Powers.Modular

-- Smoothness bound for @n@, computed as @exp(0.5 * sqrt(ln(n) * ln(ln(n))))@.
smoothnessBound :: Integer -> Integer
smoothnessBound =
  ceiling . exp . (* 0.5) . sqrt . liftM2 (*) log (log . log) . fromInteger

-- Find maximal @p@ such that @prime ^ p@ divides @n@
factorPower :: Integer -> Integer -> Integer
factorPower prime 0 = 0
factorPower prime n =
  if n `mod` prime == 0 then 1 + factorPower prime (n `div` prime) else 0

-- | Find a square root modulo a prime.
--
-- Given an integer @n@ and an odd prime @p@, such that
-- @n@ is a quadratic nonresidue modulo @p@, solve @x^2 = n (mod p)@.
--
-- Implementation follows the Algorithm 2.3.8 as described in
-- Prime Numbers: A Computational Perspective (2nd Edition) by Crandall, Pomerance
sqrtMod :: Integer -> Integer -> Integer
sqrtMod n p = ap min (p -) . (`mod` p) $ case p `mod` 8 of
  1 -> tonelliShanks n p
  3 -> lagrange n p
  5 -> case5mod8 n p
  7 -> lagrange n p
  _ -> error $ "sqrtMod with n=" ++ show n ++ ", p=" ++ show p -- TODO: handle gracefully?
 where
  tonelliShanks n p =
    let s    = factorPower 2 (p - 1) -- TODO: Calculate (s,t) using shiftToOddCount in Math.NumberTheory.Utils
        t    = (p - 1) `div` (2 ^ s)
        d    = head $ filter ((MinusOne ==) . flip jacobi p) [2 ..]
        bigA = powMod n t p
        bigD = powMod d t p
        m    = foldl'
          (\m' i ->
            let term = powMod (bigA * bigD ^ m') (2 ^ (s - 1 - i)) p
            in  m' + if term == p - 1 then 2 ^ i else 0
          )
          0
          [0 .. s - 1]
    in  powMod n ((t + 1) `div` 2) p * powMod bigD (m `div` 2) p

  lagrange n p = powMod n ((p + 1) `div` 4) p

  case5mod8 n p =
    let x = powMod n ((p + 3) `div` 8) p
        c = powMod x 2 p
    in  x * if c /= n `mod` p then powMod 2 ((p - 1) `div` 4) p else 1

reducedPrimeBase' :: Integer -> [Prime Integer]
reducedPrimeBase' n =
  nextPrime 2
    : [ prime
      | prime <- [nextPrime 3 ..]
      , jacobi n (unPrime prime) /= MinusOne
      ]

----------------------------------------------------------------------------------------------------
--                                    Initialization stage                                        --
----------------------------------------------------------------------------------------------------

-- | Find index of an element of @xs@, that is closest in absolute value
-- to @x@. Assumes that @xs@ is ascending.
--
-- TODO: like getBestIndex, missing avoiding primes[0]=2 
bestIndex :: (Num a, Ord a) => a -> [a] -> Int
bestIndex x xs = case (listToMaybe $ reverse lt, listToMaybe ge) of
  (Nothing, Nothing) -> error "empty list"
  (Nothing, Just _ ) -> length lt
  (Just _ , Nothing) -> length lt - 1
  (Just below, Just above) ->
    length lt - if distance below < distance above then 1 else 0
 where
  (lt, ge) = span (< x) xs
  distance = abs . (x -)

-- | Find the element of @xs@ that is closest to @x@ in absolute value.
--
-- TODO: like findFreeQIndex, missing checks for k divisibility and primeBaseSize
findClosestAvailable :: Int -> IntSet.IntSet -> Int
findClosestAvailable x xs =
  head $ filter (`IntSet.notMember` xs) $ x : concatMap
    (\y -> x + y : [ x - y | x > y ])
    [1 ..]


closestNotElem :: Int -> [Int] -> Int
closestNotElem x xs = head $ filter (`notElem` xs) $ x : concatMap
  (\y -> x + y : [ x - y | x > y ])
  [1 ..]

computeAParameter :: Int -> PrimeBase -> [Int] -> PolyAParam
computeAParameter qCount primeBase wantedIndexes =
  let
    bestA       = round $ sqrt (fromIntegral $ 2 * nn) / 6400 --3204754458
--    wantedIndexs = -- TODO: guess indexes, rename qCount (to k)
--      let wis' = [56, 39, 14] -- 19
--      in  if length wis' /= qCount - 1
--            then error
--              (  "qCount: Expected "
--              ++ show (length wis' + 1)
--              ++ ", but got "
--              ++ show qCount
--              )
--            else wis'

    usedIndexes = foldr (ap (flip IntSet.insert) . findClosestAvailable)
                        IntSet.empty
                        wantedIndexes

    primesProduct :: IntSet.IntSet -> Integer
    primesProduct =
      IntSet.foldr ((*) . unPrime . fst . (base primeBase IntMap.!)) 1

    -- The last prime factor is determined such that the computed @a@
    -- best matches the given @best_a@.
    lastIndex = findClosestAvailable
      (bestIndex
        (bestA `div` primesProduct usedIndexes)
        (map (unPrime . fst . snd) (IntMap.toAscList $ base primeBase))
      )
      usedIndexes

    indexes                = IntSet.insert lastIndex usedIndexes
    a                      = primesProduct indexes -- 3335815141

    (aFacts, filteredBase) = IntMap.partitionWithKey
      (const . flip IntSet.member indexes)
      (base primeBase)

    extendedFilteredBase = IntMap.map
      (\(p, s) ->
        let
          aInv = modInverse a $ unPrime p
          baInvRow =
            map ((`mod` unPrime p) . ((aInv * 2) *)) . take (qCount - 1) $ bs
        in
          (p, s, aInv, baInvRow, logP (unPrime p))
      )
      filteredBase

    bs = foldr
      (\(factor, sqrtFactor) ->
        let a'    = a `div` unPrime factor
            gamma = sqrtFactor * modInverse a' (unPrime factor)
        in  (a' * gamma :)
      )
      []
      aFacts
  in
    PolyAParam extendedFilteredBase
               bs
               a
               (map (unPrime . fst) $ IntMap.elems aFacts)

polyGen :: Integer -> [Polynomial]
polyGen n =
  let qCount        = 4 -- estimate based on bitLength(n)
      bParamsCount  = 2 ^ (qCount - 1)
      primeBase     = reducedPrimeBase n
      wantedIndexes = [[56, 39, 14], [28, 76, 72]]
      polyA         = computeAParameter qCount primeBase
      sols          = concatMap
        (flip (scanl' nextPolySolution . firstPolySolution)
              [1 .. bParamsCount - 1]
        )
        (map (computeAParameter qCount primeBase) wantedIndexes)
        --[polyA] --TODO: iterate (?) with randomness
  in  sols

data PolyAParam = PolyAParam { filteredBase :: IntMap.IntMap (Prime Integer, Integer, Integer, [Integer], Integer), bs :: [Integer], aParam :: Integer, aFacts :: [Integer] }
data PolyBParam = PolyBParam { bParam :: Integer, vBParam :: Int, direction :: Bool }
data Polynomial = Polynomial { a :: PolyAParam, b :: PolyBParam,  solutions :: IntMap.IntMap (Prime Integer, Integer, Integer, Integer) }

type FactorData = (Prime Integer, Integer)

data PrimeBase = PrimeBase { n :: Integer, base :: IntMap.IntMap FactorData } deriving (Show)

reducedPrimeBase :: Integer -> PrimeBase
reducedPrimeBase n =
  PrimeBase n
    $ IntMap.fromAscList
    . take (fromIntegral $ smoothnessBound n - 1)
    . zip [0 ..]
    $ (nextPrime 2, n `mod` 2)
    : [ (prime, sqrtMod n $ unPrime prime)
      | prime <- [nextPrime 3 ..]
      , jacobi n (unPrime prime) /= MinusOne
      ]

-- like computeFirstXArrays
firstPolySolution :: PolyAParam -> Polynomial
firstPolySolution polyAParam = Polynomial polyAParam polyBParam sols
 where
  polyBParam = PolyBParam (sum $ bs polyAParam) 1 False
  sols =
    -- TODO: shouldn't be skipping the solutions for p=2 (rewritten here with 0s)
    IntMap.adjust
        (\_ ->
          let (p, _, _, _, logp) = filteredBase polyAParam IntMap.! 0
          in  (p, logp, 0, 0)
        )
        0
      $ IntMap.map
          (\(p, t, aInv, _, logp) ->
            let
              x1 = aInv * (t - bParam polyBParam) `mod` unPrime p
              x2 = if t > 0
                then aInv * (-t - bParam polyBParam) `mod` unPrime p
                else x1
            in
              (p, logp, x1, x2)
          )
          (filteredBase polyAParam)

-- like computeNextXArrays
nextPolySolution :: Polynomial -> Integer -> Polynomial
nextPolySolution prevPolynomial i = Polynomial (a prevPolynomial)
                                               polyBParam
                                               sols
 where
  polyBParam =
    let
      v   = fromIntegral $ 1 + factorPower 2 i
      dir = odd $ i `div` (2 ^ v)
      change =
        (if dir then 1 else (-1)) * 2 * (bs (a prevPolynomial) !! (v - 1))
    in
      PolyBParam (bParam (b prevPolynomial) + change) v dir

  sols = IntMap.mapWithKey
    (\k (p, logp, oldx1, oldx2) ->
      let v                    = vBParam polyBParam
          change               = if direction polyBParam then (-1) else 1
          (_, _, _, bainvs, _) = filteredBase (a prevPolynomial) IntMap.! k
          bainv                = bainvs !! max 0 (v - 1)
          x1                   = (oldx1 + change * bainv) `mod` unPrime p
          x2                   = (oldx2 + change * bainv) `mod` unPrime p
      -- TODO: shouldn't be skipping the solutions for p=2
      in  if unPrime p == 2 then (p, logp, 0, 0) else (p, logp, x1, x2)
    )
    (solutions prevPolynomial)

-- | Find modular inverse of @a@ with regards to @modulus@.
--
-- TODO: Figure out how to use inverse in Math.NumberTheory.Moduli.Class instead?
--
-- Implementation follows the Euclid's extended algorithm as described in Algorithm 2.1.4 in
-- Prime Numbers: A Computational Perspective (2nd Edition) by Crandall, Pomerance
modInverse :: Integral a => a -> a -> a
modInverse a modulus = if signum a == parity
  then modulus - inverse
  else inverse
 where
  (inverse, parity) = step (abs a) modulus 0 1 1

  step dividend divisor u v parity = if divisor <= 1
    then (u, parity)
    else step divisor rem (q * u + v) u (-parity)
    where (q, rem) = quotRem dividend divisor

--logP :: Integer -> Integer
logP p = floor $ 0.5 + 4.3987694 * log (fromIntegral p)

----------------------------------------------------------------------------------------------------
--                                          Sieve stage                                           --
----------------------------------------------------------------------------------------------------

sieve :: [Polynomial] -> [(Polynomial, [Int])]
sieve sols =
  let
    sieveArraySize = 6400 -- TODO: derive from input
    sieveWith :: Integer -> Integer -> Integer -> IntMap.IntMap Int8
    sieveWith p logP xMin =
      let positions =
              takeWhile (< sieveArraySize)
                . map fromIntegral
                $ [xMin, xMin + p ..]
      in  IntMap.fromAscList [ (pos, fromIntegral logP) | pos <- positions ]

    makeSieve
      :: (Prime Integer -> Integer -> Integer)
      -> Polynomial
      -> IntMap.IntMap Int8
    makeSieve startOffset =
      IntMap.unionsWith (+)
        . (IntMap.fromList [ (i, 0) | i <- [0 .. sieveArraySize - 1] ] :)
        . concatMap
            (\(p, logp, x1, x2) ->
              sieveWith (unPrime p) logp (startOffset p x1)
                : [ sieveWith (unPrime p) logp (startOffset p x2)
                  | unPrime p /= 2 && x1 /= x2
                  ]
            )
        . solutions

    filterSmoothKeys :: (Int -> Int) -> IntMap.IntMap Int8 -> IntMap.IntMap Int
    filterSmoothKeys dir = IntMap.mapMaybeWithKey
      (\k x -> if x < 0 && x > (-128) then Just (dir k) else Nothing)

    mergeSmooths :: [Int] -> [Int] -> [Int]
    mergeSmooths [] xs = xs
    mergeSmooths xs [] = xs
    mergeSmooths xs@(x : xs') ys@(y : ys')
      | abs x < abs y = x : mergeSmooths xs' ys
      | otherwise     = y : mergeSmooths xs ys'

    solWithSmooths :: [Polynomial] -> [(Polynomial, [Int])]
    solWithSmooths = map
      (\sol ->
        let
          smoothPos =
            IntMap.elems . filterSmoothKeys id $ makeSieve (const id) sol
          smoothNeg = IntMap.elems . filterSmoothKeys negate $ makeSieve
            ((-) . unPrime)
            sol
        in
          (sol, mergeSmooths smoothPos smoothNeg)
      )
  in
    solWithSmooths sols

----------------------------------------------------------------------------------------------------
--                                     Trial division stage                                       --
----------------------------------------------------------------------------------------------------

aqPairTest q poly =
  let pass1 x = aFacts (a poly) ++ IntMap.foldlWithKey'
        (\ps k (p, _, x1, x2) ->
          let xModP = fromIntegral x `mod` unPrime p
              add   = xModP == x1 || xModP == x2
          in  if add then unPrime p : ps else ps
        )
        []
        -- TODO: stop skipping Prime 2. now 2 is skipped in when creating polynomials. so it is skipped here too. 
        (IntMap.withoutKeys (solutions poly) (IntSet.singleton 0))

      facOut num den =
          let (qt, rm) = num `quotRem` den
          in  if rm == 0
                then let (a, b) = facOut qt den in (a, 1 + b)
                else (num, 0)

      --pass2 :: [Integer] -> (Integer, [Integer])
      pass2 =
          foldr
              (\p (qRest, exps) ->
                let (qt, pw) = facOut qRest (fromIntegral p)
                in  if pw == 0 then (qt, exps) else (qt, (p, pw) : exps)
              )
              (abs q, [ (-1, 1) | q < 0 ])
            . (2 :) --TODO: see above (also, shouldn't be checking pw ==0, because it should never hold)
  in  pass2 . pass1

-- | Collect perfectly smooth congurences.
--
-- TODO: collect partial congruences too.
aqListTest :: [(Polynomial, [Int])] -> [AQPair]
aqListTest =
  let
    --maxQRest = 16280.473117728447

    smoothPairs pol smoothX =
      let bigA                  = aParam (a pol) * smoothX + bParam (b pol)
          bigQ                  = bigA ^ 2 - nn
          (qRest, smallFactors) = aqPairTest bigQ pol smoothX
      in  if qRest == 1 then Just (AQPair bigA smallFactors) else Nothing

    filterSmoothXs :: Polynomial -> [Int] -> [AQPair]
    filterSmoothXs poly = mapMaybe (smoothPairs poly . fromIntegral) . reverse

    requiredSmoothCongrueceCount =
      (10 +)
        . ap ((+) . IntMap.size . solutions) (length . aFacts . a)
        . fst
        . head
  in
    --map (uncurry filterSmoothXs) . sieve . polyGen $ nn
    liftM2 take requiredSmoothCongrueceCount
      $ concatMap (uncurry filterSmoothXs)

----------------------------------------------------------------------------------------------------
--                                     Linear algebra stage                                       --
----------------------------------------------------------------------------------------------------

addToColumn2RowMap :: AQPair -> IntMap.IntMap [AQPair] -> IntMap.IntMap [AQPair]
addToColumn2RowMap cong oddExpCongs = foldl'
  (\oec (q, exp) -> if odd exp
    then IntMap.insertWith (++) (fromIntegral q) [cong] oec
    else oec
  )
  oddExpCongs
  (factors cong)


filterSingletons
  :: [AQPair]
  -> IntMap.IntMap [AQPair]
  -> (IntMap.IntMap [AQPair], [AQPair], Bool)
filterSingletons congs oddExpCongs = foldl'
  (\(oec, remainingCongs, shrunk) cong -> if or $ hasSingleton cong oec
    then (removeCongruence cong oec, remainingCongs, True)
    else (oec, cong : remainingCongs, shrunk)
  )
  (oddExpCongs, [], False)
  congs
 where
  hasSingleton cong oec =
    let oddFactors =
            map (fromIntegral . fst) . filter (odd . snd) . factors $ cong
    in  map ((== 1) . length . fromMaybe [] . (oec IntMap.!?)) oddFactors
  removeCongruence :: AQPair -> IntMap.IntMap [AQPair] -> IntMap.IntMap [AQPair]
  removeCongruence cong oddExpCongs = foldr
    (\(fac, pow) oec -> if odd pow
      then IntMap.updateWithKey
        (\k congsAtK -> if length congsAtK == 1 && head congsAtK == cong
          then Nothing
          else Just $ filter (/= cong) congsAtK
        )
        (fromIntegral fac)
        oec
      else oec
    )
    oddExpCongs
    (factors cong)

totalFilter polsWithXs =
  let aqs                       = aqListTest polsWithXs
      before                    = foldr addToColumn2RowMap IntMap.empty aqs

      (finalMap, finalCongs, _) = until
        (\(_, _, shrunk) -> not shrunk)
        (\(oec, aqps, _) -> filterSingletons aqps oec)
        (before, aqs, True)
  in  (finalCongs, finalMap)

totalFilter' polsWithXs =
  let requiredSmoothCongrueceCount =
          (10 +)
            . ap ((+) . IntMap.size . solutions) (length . aFacts . a)
            . fst
            . head
            $ polsWithXs
  in  requiredSmoothCongrueceCount

data AQPair = AQPair { bigA :: Integer, factors :: [(Integer, Integer)] } deriving (Eq)
data Row = Row { rowId :: Int, columnIndexes :: IntSet.IntSet, rowHistory :: IntSet.IntSet } deriving (Show)

-- | Use Gaussian elimination to find 
solve polsWithXs =
  let
    (finalCongs, finalMap) = totalFilter polsWithXs

    factor2ColumnIndexMap =
      snd $ IntMap.mapAccum (\i _ -> (i + 1, i)) 0 finalMap

    matrix = snd . mapAccumL
      (\rowIndex cong ->
        let
          oddFactors =
            map (fromIntegral . fst) . filter (odd . snd) . factors $ cong
          colIndexes =
            IntSet.fromList $ map (factor2ColumnIndexMap IntMap.!) oddFactors
        in
          (rowIndex + 1, Row rowIndex colIndexes (IntSet.singleton rowIndex))
      )
      0

    symdiff x y = IntSet.union (x IntSet.\\ y) (y IntSet.\\ x)
    addXor :: Row -> Row -> Row
    addXor r piv = Row (rowId r)
                       (symdiff (columnIndexes r) (columnIndexes piv))
                       (symdiff (rowHistory r) (rowHistory piv))

    congsMatrix = matrix finalCongs
    sol         = snd $ foldl'
      (\(rows, totalAQs) _ ->
        let
          (pr       , rest   ) = pivotAndRest rows
          (emptyRows, newRows) = pivotAdded pr rest
          taqs =
            map (map (finalCongs !!) . IntSet.toAscList . rowHistory) emptyRows
        in
          (newRows, totalAQs ++ taqs)
      )
      (congsMatrix, [])
      (replicate (length congsMatrix) ())

    maxColIndex = IntSet.findMax . columnIndexes

    pivot =
      fst
        . foldr1 (\(a, ai) (b, bi) -> if ai > bi then (a, ai) else (b, bi))
        . map (\row -> (row, maxColIndex row))

    pivotAndRest rows =
      let pivotRow = pivot rows
      in  (pivotRow, filter ((/= rowId pivotRow) . rowId) rows)

    pivotAdded pivotRow rest =
      let pivotColIndex = maxColIndex pivotRow
      in
        partition (IntSet.null . columnIndexes) $ map
          (\row -> if maxColIndex row == pivotColIndex
            then addXor row pivotRow
            else row
          )
          rest
  in
    sol

testForFactor bigN aqPairs =
  let
    incExps addedExps Nothing       = Just addedExps
    incExps addedExps (Just exExps) = Just (addedExps + exExps)

    addAQPair aqPair totalFacts =
      foldr (\(p, exp) -> IntMap.alter (incExps exp) (fromIntegral p))
            totalFacts
        . factors
        $ aqPair

    totalFactors = foldr addAQPair IntMap.empty aqPairs
    totalQsqrt =
      IntMap.foldrWithKey
          (\p exp prod -> if odd exp
            then error "not a solution -> gaussian elimination should continue"
            else (prod * fromIntegral p ^ (exp `div` 2)) `mod` bigN
          )
          1
        $ IntMap.withoutKeys totalFactors (IntSet.singleton (-1))

    totalAprod =
      foldr (\aqPair prod -> (prod * bigA aqPair) `mod` bigN) 1 aqPairs

    fact =
      let minusGcd = gcd (totalAprod - totalQsqrt) bigN
          plusGcd  = gcd (totalAprod + totalQsqrt) bigN
      in  if minusGcd > 1 && minusGcd < bigN
            then Just minusGcd
            else if plusGcd > 1 && plusGcd < bigN then Just plusGcd else Nothing -- error "no factor"
  in
    fact

factorise =
  let polsWithXs = sieve . polyGen $ nn
  in  fromJust . head . dropWhile isNothing . map (testForFactor nn) $ solve
        polsWithXs

nn :: Integer
nn = 210338839227361475450591909


