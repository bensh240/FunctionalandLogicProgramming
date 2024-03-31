{-# LANGUAGE InstanceSigs #-}

module State (State (..), get, gets, set, modify, execState, evalState) where

import Control.Applicative (liftA2)

newtype State s a = State {runState :: s -> (a, s)}
instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap f st = State $ \s ->
        let (a, s2) = runState st s
         in (f a, s2)

instance Applicative (State s) where
    pure :: a -> State s a
    pure a = State $ \s -> (a, s)
    liftA2 ::
        (a -> b -> c) ->
        State s a ->
        State s b ->
        State s c
    liftA2 f sa sb = State $ \s ->
        let (a, s2) = runState sa s
            (b, s3) = runState sb s2
         in (f a b, s3)

instance Monad (State s) where
    (>>=) :: State s a -> (a -> State s b) -> State s b
    sa >>= f = State $ \s ->
        let (a, s2) = runState sa s
         in runState (f a) s2

get :: State s s
get = State $ \s -> (s, s)

gets :: (s -> a) -> State s a
gets f = fmap f get -- Or (`fmap` get)

set :: s -> State s ()
set s = State $ const ((), s)

modify :: (s -> s) -> State s ()
modify f = do
    s <- get
    set $ f s

(.:) :: (t1 -> t2) -> (t3 -> t4 -> t1) -> t3 -> t4 -> t2
f .: g = \x y -> f $ g x y

evalState :: State s a -> s -> a
evalState = fst .: runState
execState :: State s a -> s -> s
execState = snd .: runState
