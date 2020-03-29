module Questions1 where

-- Problem 1
--- Find the last element of a list.
myLast :: [a] -> a
myLast []       = error "\"myLast\" does not work on empty lists!"
myLast [x     ] = x
myLast (_ : xs) = myLast xs

myLast' :: [a] -> a
myLast' [] = error "\"myLast'\" does not work on empty lists!"
myLast' xs = xs !! (length xs - 1)

-- Problem 2
--- Find the last but one element of a list.
myButLast :: [a] -> a
myButLast []       = error "\"myButLast\" does not work on empty lists!"
myButLast [_] = error "\"myButLast\" does not work on single element lists!"
myButLast [x, _]   = x
myButLast (_ : xs) = myButLast xs

-- Problem 3
--- Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt xs k | null xs   = error "List cannot be empty!"
               | k < 1     = error "Index of element has to be positive!"
               | otherwise = internalElementAt xs k
 where
  internalElementAt :: [a] -> Int -> a
  internalElementAt []        _  = error "Index is out of range!"
  internalElementAt (x : _  ) 1  = x
  internalElementAt (_ : _xs) _k = internalElementAt _xs (_k - 1)

elementAt' :: [a] -> Int -> a
elementAt' xs k = xs !! (k - 1)

-- Problem 4
--- Find the number of elements of a list.
myLength :: [a] -> Int
myLength []       = 0
myLength (_ : xs) = 1 + myLength xs

myLength' :: [a] -> Int
myLength' = foldl (\len _ -> len + 1) 0

myLength'' :: [a] -> Int
myLength'' = foldr (const (+ 1)) 0

-- Problem 5
--- Reverse a list.
myReverse :: [a] -> [a]
myReverse []       = []
myReverse (x : xs) = myReverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

-- Problem 6
--- Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: Eq a => [a] -> Bool
isPalindrome = _isPalindrome []
 where
  _isPalindrome []      []       = True
  _isPalindrome (_ : _) []       = False
  _isPalindrome []      (x : xs) = _isPalindrome [x] xs
  _isPalindrome (x : xs) (y : ys) =
    (x : xs) == ys || xs == ys || _isPalindrome ([y, x] ++ xs) ys

isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' xs = reverse xs == xs

isPalindrome'' :: Eq a => [a] -> Bool
isPalindrome'' xs =
  foldl (\acc (x, y) -> acc && x == y) True (zip xs (reverse xs))

-- Problem 7
--- Flatten a nested list structure.
--- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x ) = [x]
flatten (List xs) = concatMap flatten xs

flatten' :: NestedList a -> [a]
flatten' (Elem x ) = [x]
flatten' (List xs) = foldr ((++) . flatten) [] xs

-- Problem 8
--- Eliminate consecutive duplicates of list elements.
--- If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
compress :: Eq a => [a] -> [a]
compress []       = []
compress (x : xs) = x : filteredRest x xs
 where
  filteredRest :: Eq a => a -> [a] -> [a]
  filteredRest _ [] = []
  filteredRest _x (y : ys) =
    if _x == y then filteredRest _x ys else y : filteredRest y ys

compress' :: Eq a => [a] -> [a]
compress' (x : xs@(y : _)) | x == y    = compress xs
                           | otherwise = x : compress xs
compress' ys = ys

compress'' :: Eq a => [a] -> [a]
compress'' []       = []
compress'' (x : xs) = x : compress (dropWhile (== x) xs)

-- Problem 9
--- Pack consecutive duplicates of list elements into sub-lists. If a list contains repeated elements they should be placed in separate sub-lists.
pack :: Eq a => [a] -> [[a]]
pack xs = foldl
  (\acc x ->
    if head (last acc) == x then init acc ++ [x : last acc] else acc ++ [[x]]
  )
  [[head xs]]
  (tail xs)

pack' :: Eq a => [a] -> [[a]]
pack' []         = []
pack' xs@(x : _) = let (prefix, rest) = span (== x) xs in prefix : pack' rest

-- Problem 10
--- Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
encode :: Eq a => [a] -> [(Int, a)]
encode xs = [ (length x, head x) | x <- pack' xs ]
