{-# LANGUAGE TypeSynonymInstances #-}

module Calc where
import           ExprT
import           Parser
import qualified StackVM                       as S

eval :: ExprT -> Integer
eval (Lit n    ) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

evalStr :: String -> Maybe Integer
evalStr s = eval <$> parseExp Lit Add Mul s

class Expr a where
        lit :: Integer -> a
        add :: a -> a -> a
        mul :: a -> a -> a

instance Expr ExprT where
        lit n = Lit n
        add e1 e2 = Add e1 e2
        mul e1 e2 = Mul e1 e2

instance Expr Integer where
        lit n = n
        add e1 e2 = e1 + e2
        mul e1 e2 = e1 * e2

instance Expr Bool where
        lit n = n > 0
        add e1 e2 = e1 || e2
        mul e1 e2 = e1 && e2

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
        lit n = MinMax n
        add (MinMax n) (MinMax m) = MinMax $ max n m
        mul (MinMax n) (MinMax m) = MinMax $ min n m

instance Expr Mod7 where
        lit n = Mod7 $ n `mod` 7
        add (Mod7 n) (Mod7 m) = Mod7 $ (n + m) `mod` 7
        mul (Mod7 n) (Mod7 m) = Mod7 $ (n * m) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

instance Expr S.Program where
        lit n = S.PushI n
        add e1 e2 = e1 ++ e2 ++ [S.Add]
        mul e1 e2 = e1 ++ e2 ++ [S.Mul]
