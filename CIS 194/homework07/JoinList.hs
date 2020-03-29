module JoinList where

import           Data.Monoid
import           Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
        deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _  ) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
j1 +++ j2 = Append (tag j1 `mappend` tag j2) j1 j2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i _ | i < 0              = Nothing
indexJ _ Empty                  = Nothing
indexJ 0 (Single _ a)           = Just a
indexJ _ (Single _ _)           = Nothing
indexJ i (Append b _ _) | i > b = Nothing
indexJ i (Append b j1 j2)       = indexJ i j1 <|> indexJ i j2
