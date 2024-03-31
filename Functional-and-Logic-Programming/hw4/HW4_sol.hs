{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Werror #-}
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
import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Foldable (foldMap, foldl, foldr), Functor (fmap), Int, Maybe (..), Monoid (..), Num (..), Ord (..), Ordering (..), Semigroup (..), Show (..), String, all, and, any, concat, concatMap, const, curry, drop, dropWhile, elem, error, filter, flip, fst, id, init, map, not, or, product, replicate, reverse, snd, take, takeWhile, uncurry, undefined, zip, zipWith, (!!), ($), (&&), (++), (.), (||))

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
elem x = getAny . foldMap (Any . (x ==))
find :: Foldable t => (a -> Bool) -> t a -> Maybe a
find p = getFirst . foldMap (\x -> First $ if p x then Just x else Nothing)
length :: Foldable t => t a -> Int
length = getSum . foldMap (const $ Sum 1)
null :: Foldable t => t a -> Bool
null = getAll . foldMap (All . const False)
maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = fmap getMax . foldMap (Just . Max)
maxBy :: (Foldable t, Ord b) => (a -> b) -> t a -> Maybe a
maxBy f = fmap (getArg . getMax) . foldMap (\a -> Just $ Max $ Arg (f a) a)
minimum = fmap getMin . foldMap (Just . Min)
minimum :: (Foldable t, Ord a) => t a -> Maybe a
minBy :: (Foldable t, Ord b) => (a -> b) -> t a -> Maybe a
minBy f = fmap (getArg . getMin) . foldMap (\a -> Just $ Min $ Arg (f a) a)
sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum
product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

getArg :: Arg a b -> b
getArg (Arg _ b) = b

-- Section 2: Functor functions
fmapToFst :: Functor f => (a -> b) -> f a -> f (b, a)
fmapToFst f = fmap (\x -> (f x, x))
fmapToSnd :: Functor f => (a -> b) -> f a -> f (a, b)
fmapToSnd f = fmap (\x -> (x, f x))
strengthenR :: Functor f => b -> f a -> f (a, b)
strengthenR b = fmap (\x -> (x, b))
strengthenL :: Functor f => b -> f a -> f (b, a)
strengthenL b = fmap (\x -> (b, x))
unzip :: Functor f => f (a, b) -> (f a, f b)
unzip fa = (fmap fst fa, fmap snd fa)
coUnzip :: Functor f => Either (f a) (f b) -> f (Either a b)
coUnzip = \case
    Right xs -> fmap Right xs
    Left xs -> fmap Left xs

-- Section 3.1
class Unfoldable t where
    fromList :: [a] -> t a
    fromList = unfoldr uncons
    unfoldr :: (b -> Maybe (a, b)) -> b -> t a
    unfoldr f = fromList . go . f
      where
        go = maybe [] (\(a, b) -> a : go (f b))
    {-# MINIMAL fromList | unfoldr #-}

-- Section 3.2
instance Unfoldable [] where
    fromList = id
instance Unfoldable PersistentArray where
    fromList = foldr PA.pushr PA.empty . reverse
instance Unfoldable Deque where
    fromList = foldr DQ.pushl DQ.empty

-- Section 4
instance Foldable Deque where
    foldMap f = foldMap f . toList'
      where
        toList' = maybe [] (\(x, dq') -> x : toList' dq') . DQ.popl
instance Functor Deque where
    fmap f = fromList . map f . toList
instance Semigroup (Deque a) where
    a1 <> a2 = fromList $ toList a1 <> toList a2
instance Monoid (Deque a) where
    mempty = DQ.empty

instance Foldable PersistentArray where
    foldMap f = foldMap f . toList'
      where
        toList' pa = map (fromJust . (`PA.lookup` pa)) [0 .. PA.length pa - 1]
instance Functor PersistentArray where
    fmap f = fromList . map f . toList
instance Semigroup (PersistentArray a) where
    a1 <> a2 = fromList $ toList a1 <> toList a2
instance Monoid (PersistentArray a) where
    mempty = PA.empty

-- Section 5
newtype ZipList a = ZipList {getZipList :: [a]} deriving (Show, Eq)
instance Semigroup a => Semigroup (ZipList a) where
    ZipList [] <> _ = ZipList []
    _ <> ZipList [] = ZipList []
    ZipList (x : xs) <> ZipList (y : ys) =
        ZipList $ x <> y : getZipList (ZipList xs <> ZipList ys)
instance Monoid a => Monoid (ZipList a) where
    mempty = ZipList $ map (const mempty) [0 ..] -- or import repeat
