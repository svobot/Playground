fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs) | even x    = (x - 2) * fun1 xs
              | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (+ (-2)) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = last . takeWhile (> 1) . iterate
        (\n -> if even n then n `div` 2 else 3 * n + 1)

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertNode Leaf
    where
        insertNode x Leaf = Node 0 Leaf x Leaf
        insertNode x (Node n Leaf y left) =
                Node (n + 1) (insertNode x Leaf) y left
        insertNode x (Node n right y Leaf) = Node n right y (insertNode x Leaf)
        insertNode x (Node n right@(Node m1 _ _ _) y left@(Node m2 _ _ _)) =
                if m1 > m2
                        then Node n right y (insertNode x left)
                        else
                                let newRight@(Node m1' _ _ _) =
                                            insertNode x right
                                in  Node
                                            (if m1 == m1' then n else n + 1)
                                            newRight
                                            y
                                            left

xor :: [Bool] -> Bool
xor = foldr (\b acc -> if b then not acc else acc) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base $ reverse xs

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+ 1) . (* 2)) $ filter
        (`notElem` ([ i + j + 2 * i * j
                    | i <- [1 .. n]
                    , j <- [1 .. i]
                    , i + j + 2 * i * j <= n
                    ]
                   )
        )
        [1 .. n]
