{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wall -Werror #-}
-- Needed for adding instances outside the data definition module.
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW4 where

import Data.Either
import Data.List (find, foldl', uncons)
import Data.Map (Map, (!?))
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid (All (..), Any (..), First (..), Last (..), Product (..), Sum (..))
import Data.Semigroup (Arg (..), Max (..), Min (..))
import Data.Set (Set)
import qualified Data.Set as S
import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Foldable (foldMap, foldl, foldr), Functor (fmap), Int, Maybe (..), Monoid (..), Num (..), Ord (..), Ordering (..), Semigroup (..), Show (..), String, all, and, any, concat, concatMap, const, curry, drop, dropWhile, error, filter, flip, fst, id, init, map, not, or, replicate, reverse, snd, take, takeWhile, uncurry, undefined, zip, zipWith, (!!), ($), (&&), (++), (.), (||))

import Deque (Deque)
import qualified Deque as DQ
import PersistentArray (PersistentArray)
import qualified PersistentArray as PA

-- Section 1: Foldable functions
fold :: (Foldable t, Monoid a) => t a -> a
fold = foldMap id
toList :: Foldable t => t a -> [a]
toList = foldMap (: [])
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x t = case Data.List.find (==x) (toList t) of
    Nothing -> False
    Just _ -> True
find :: (Foldable t, Eq a) => (a -> Bool) -> t a -> Maybe a
find p t = Data.List.find p (toList t)
length :: Foldable t => t a -> Int
length = foldr (\_ y -> y+1) 0
null :: Foldable t => t a -> Bool
null t = length t == 0
maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum t = if null t then Nothing else Just $ foldr go l t
    where
        (l:_) = toList t
        go x y = if x > y then x else y
maxBy :: (Foldable t, Ord b) => (a -> b) -> t a -> Maybe a
maxBy f t = if null t then Nothing else Just maxX
    where
        Just (Arg _ maxX) = maximum [Arg (f x) x | x <- toList t]
minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum t = if null t then Nothing else Just $ foldr go l t
    where
        (l:_) = toList t
        go x y = if x > y then y else x
minBy :: (Foldable t, Ord b) => (a -> b) -> t a -> Maybe a
minBy f t = if null t then Nothing else Just maxX
    where
        Just (Arg _ maxX) = minimum [Arg (f x) x | x <- toList t]
sum :: (Foldable t, Num a) => t a -> a
sum t = if null t then 0 else getSum $ foldMap Sum t
product :: (Foldable t, Num a) => t a -> a
product t = if null t then 0 else getProduct $ foldMap Product t

-- -- Section 2: Functor functions
fmapToFst :: Functor f => (a -> b) -> f a -> f (b, a)
fmapToFst f  = fmap (\x -> (f x, x))
fmapToSnd :: Functor f => (a -> b) -> f a -> f (a, b)
fmapToSnd f = fmap (\x -> (x, f x))
strengthenL :: Functor f => b -> f a -> f (b, a)
strengthenL x = fmapToFst $ const x
strengthenR :: Functor f => b -> f a -> f (a, b)
strengthenR x = fmapToSnd $ const x
unzip :: Functor f => f (a, b) -> (f a, f b)
unzip f = (fmap fst f, fmap snd f)
coUnzip :: Functor f => Either (f a) (f b) -> f (Either a b)
coUnzip (Left f)  = fmap Left f
coUnzip (Right f) = fmap Right f

-- Section 3: Unfodlable
class Unfoldable t where
    fromList :: [a] -> t a
    fromList l = unfoldr go l
        where
            go :: [a] -> Maybe (a, [a])
            go l' = case l' of
                [] -> Nothing
                (x:xs) -> Just (x,xs)
    unfoldr :: (b -> Maybe (a, b)) -> b -> t a
    unfoldr f seed = fromList $ go f seed
        where
            go :: (b -> Maybe (a, b)) -> b -> [a]
            go f' seed' = case f' seed' of
                Nothing -> []
                Just (x, y) -> x : go f' y
    {-# MINIMAL fromList | unfoldr #-}

instance Unfoldable [] where
    fromList = id
instance Unfoldable Deque where
    fromList = foldr DQ.pushl DQ.empty
instance Unfoldable PersistentArray where
    fromList = foldl' (flip PA.pushr) PA.empty

-- Section 4: Data structure instances
instance Foldable Deque where
    foldr :: (a -> b -> b) -> b -> Deque a -> b
    foldr f seed dq = case DQ.popl dq of
        Nothing -> seed
        Just (newSeed, newDQ) -> f newSeed (foldr f seed newDQ)
instance Functor Deque where
    fmap :: (a -> b) -> Deque a -> Deque b
    fmap f dq = fromList (fmap f (toList dq))
instance Semigroup (Deque a) where
    (<>) :: Deque a -> Deque a -> Deque a
    (<>) dq1 dq2 = fromList $ toList dq1 ++ toList dq2
instance Monoid (Deque a) where
    mempty :: Deque a
    mempty = DQ.empty
instance Foldable PersistentArray where
    foldr :: (a -> b -> b) -> b -> PersistentArray a -> b
    foldr f seed pa = go f seed pa 0
        where
            go f' seed' pa' index = case PA.lookup index pa' of
                Nothing -> seed'
                Just x -> f' x (go f' seed' pa' (index+1))
instance Functor PersistentArray where 
    fmap :: (a -> b) -> PersistentArray a -> PersistentArray b
    fmap f pa = fromList (fmap f (toList pa))
instance Semigroup (PersistentArray a) where
    (<>) :: PersistentArray a -> PersistentArray a -> PersistentArray a
    (<>) pa1 pa2 = fromList $ toList pa1 ++ toList pa2
instance Monoid (PersistentArray a) where
    mempty :: PersistentArray a
    mempty = PA.empty

-- -- Bonus section
newtype ZipList a = ZipList {getZipList :: [a]} deriving (Show, Eq)

instance Semigroup a => Semigroup (ZipList a) where
    (<>) :: Semigroup a => ZipList a -> ZipList a -> ZipList a
    (<>) z1 z2 =  ZipList $ Prelude.zipWith (<>) (getZipList z1) (getZipList z2)
instance Monoid a => Monoid (ZipList a) where
    mempty :: Monoid a => ZipList a
    mempty = ZipList (mempty:mempty)
