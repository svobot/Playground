{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import           Employee
import           Data.Tree
import           Data.List

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e : es) (f + empFun e)

instance Semigroup GuestList where
        (GL gl1 f1) <> (GL gl2 f2) = GL (gl1 ++ gl2) (f1 + f2)

instance Monoid GuestList where
        mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

--treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
--treeFold e _ Empty        = e
--treeFold e f (Node l x r) = f (treeFold e f l) x (treeFold e f r)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss subtrees =
        (glCons boss $ mconcat withoutSubbosses, mconcat withSubbosses)
        where (withSubbosses, withoutSubbosses) = unzip subtrees

maxFun :: Tree Employee -> GuestList
maxFun company = max withBoss withoutBoss
        where (withBoss, withoutBoss) = foldTree nextLevel company

main :: IO ()
main = do
        contents <- readFile "company.txt"
        let company     = read contents :: Tree Employee
        let (GL gl fun) = maxFun company
        putStrLn ("Total fun: " ++ show fun)
        mapM_ putStrLn $ sort $ map empName gl
