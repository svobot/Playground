module Questions2 where

-- Problem 11
--- Modified run-length encoding.
--- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
data ListItem a = Single a | Multiple Int a deriving (Show)

pack :: Eq a => [a] -> [[a]]
pack []         = []
pack xs@(x : _) = let (prefix, rest) = span (== x) xs in prefix : pack rest

encode :: Eq a => [a] -> [(Int, a)]
encode xs = [ (length x, head x) | x <- pack xs ]

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified xs =
  [ if n == 1 then Single x else Multiple n x | (n, x) <- encode xs ]

-- Problem 12
--- Decode a run-length encoded list.
--- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
decodeModified :: [ListItem a] -> [a]
decodeModified []                  = []
decodeModified (Single y     : xs) = y : decodeModified xs
decodeModified (Multiple n y : xs) = replicate n y ++ decodeModified xs

decodeModified' :: [ListItem a] -> [a]
decodeModified' = concatMap decodeHelper
 where
  decodeHelper :: ListItem a -> [a]
  decodeHelper (Single x    ) = [x]
  decodeHelper (Multiple n x) = replicate n x

-- Problem 13
--- Run-length encoding of a list (direct solution).
--- Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sub-lists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect [] = []
encodeDirect (x : xs) | prefixSize == 1 = Single x : encodeDirect rest
                      | otherwise = Multiple prefixSize x : encodeDirect rest
 where
  (prefix, rest) = span (== x) xs
  prefixSize     = length prefix + 1

-- Problem 14
--- Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli = foldr (\x acc -> x : x : acc) []

dupli' :: [a] -> [a]
dupli' = concatMap (\x -> [x, x])

dupli'' :: [a] -> [a]
dupli'' = concatMap (replicate 2)

-- Problem 15
--- Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

repli' :: [a] -> Int -> [a]
repli' = flip $ concatMap . replicate

-- Problem 16
--- Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery xs n | length xs < n = xs
               | otherwise     = init prefix ++ dropEvery rest n
  where (prefix, rest) = splitAt n xs

dropEvery' :: [a] -> Int -> [a]
dropEvery' xs n = map snd (filter ((/= n) . fst) (zip (cycle [1 .. n]) xs))

-- Problem 17
--- Split a list into two parts; the length of the first part is given.
--- Do not use any predefined predicates.
split :: [a] -> Int -> ([a], [a])
split xs n = (strip prefix, strip rest)
 where
  zs             = zip xs (replicate n True ++ repeat False)
  (prefix, rest) = span ((== True) . snd) zs
  strip          = map fst

split' :: [a] -> Int -> ([a], [a])
split' (x : xs) n | n > 0 = let (ys, zs) = split' xs (n - 1) in (x : ys, zs)
split' xs _               = ([], xs)

-- Problem 18
--- Extract a slice from a list.
--- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice xs i j = sliceHelper xs i j 1
 where
  sliceHelper :: [a] -> Int -> Int -> Int -> [a]
  sliceHelper [] _ _ _ = [] -- useless case. handled by slice [].
  sliceHelper (_x : _xs) _i _j _k
    | _k < _i   = sliceHelper _xs _i _j (_k + 1)
    | _k <= _j  = _x : sliceHelper _xs _i _j (_k + 1)
    | otherwise = []

slice' :: [a] -> Int -> Int -> [a]
slice' xs i j = drop (i - 1) $ take j xs

-- Problem 19
--- Rotate a list N places to the left.
--- Hint: Use the predefined functions length and (++).
rotate :: [a] -> Int -> [a]
rotate xs n = rest ++ prefix
  where (prefix, rest) = splitAt (n `mod` length xs) xs

-- Problem 20
--- Remove the K'th element from a list.
removeAt :: Int -> [a] -> (a, [a])
removeAt _ [] = error "removeAt: empty list"
removeAt n (x : xs) | n > 1     = (removed, x : rest)
                    | n == 1    = (x, xs)
                    | otherwise = error "removeAt: non-positive index"
  where (removed, rest) = removeAt (n - 1) xs

removeAt' :: Int -> [a] -> (Maybe a, [a])
removeAt' _ [] = (Nothing, [])
removeAt' n xs | n < 1         = (Nothing, xs)
               | n > length xs = (Nothing, xs)
               | otherwise     = (Just (xs !! (n - 1)), init prefix ++ rest)
  where (prefix, rest) = splitAt n xs
