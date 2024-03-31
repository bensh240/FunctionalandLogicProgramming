{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Deque (Deque, empty, pushr, pushl, popr, popl) where

data Deque a = Deque [a] [a] deriving (Show, Functor)

empty :: Deque a
empty = Deque [] []

instance Foldable Deque where
    foldr f b (Deque l r) = foldr f b (l ++ reverse r)
instance Semigroup (Deque a) where
    (Deque l1 r1) <> (Deque l2 r2) = Deque (l1 ++ reverse r1) (reverse $ l2 ++ reverse r2)
instance Monoid (Deque a) where
    mempty = empty

pushl :: a -> Deque a -> Deque a
pushl x (Deque l r) = Deque (x : l) r

pushr :: a -> Deque a -> Deque a
pushr x (Deque l r) = Deque l (x : r)

popl :: Deque a -> Maybe (a, Deque a)
popl = \case
    Deque [] [] -> Nothing
    Deque (l : ls) r -> Just (l, Deque ls r)
    Deque [] r -> popl $ Deque (reverse r) []

popr :: Deque a -> Maybe (a, Deque a)
popr = \case
    Deque [] [] -> Nothing
    Deque l (r : rs) -> Just (r, Deque l rs)
    Deque l [] -> popr $ Deque [] (reverse l)
