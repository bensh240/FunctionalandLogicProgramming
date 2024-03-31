-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW1.hs should successfully compile.

-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}


module HW1 where

import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Int, Maybe (..), Num (..), Ord (..), Show (..), String, all, and, any, concat, concatMap, const, curry, elem, error, filter, flip, foldl, foldr, fst, id, length, lookup, map, not, notElem, null, or, product, reverse, snd, sum,tail, uncurry, undefined, (!!), ($), (&&), (++), (.), (||))
import Data.Bool
import Text.XHtml (navy, label, abbr)
import System.Console.GetOpt (getOpt)


fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just a) = a

maybe :: b -> (a -> b) -> Maybe a -> b
maybe b _ Nothing = b
maybe _ f (Just a) = f a

catMaybes :: [Maybe a] -> [a]
catMaybes l = case l of
    []           -> []
    (Nothing:xs) -> catMaybes xs
    (Just x:xs)  -> x : catMaybes xs

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe = catMaybes .: map

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f .: g = \x y -> f $ g x y


either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left a) = f a
either _ g (Right b)= g b

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left (f a)
mapLeft _ (Right b) = Right b

catEithers :: [Either a b] -> Either a [b]
catEithers = foldr go $ Right [] 
    where
        go (Left a) _ = Left a
        go (Right b) rest = eitherMap (b :) rest

eitherMap :: (b -> c) -> Either a b -> Either a c
eitherMap f (Right b) = Right $ f b
eitherMap _ (Left l) = Left l

concatEitherMap :: (b -> Either a c) -> Either a b -> Either a c
concatEitherMap _ (Left a)  = Left a 
concatEitherMap f (Right b) =  f b

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr go ([],[]) 
     where 
        go (Left a) (as, bs)  = (a : as, bs)
        go (Right b) (as, bs) = (as, b : bs) 





-- >>> foldr (++) "f" "abcde"
-- Couldn't match type ‘Char’ with ‘[Char]’
-- Expected type: [[Char]]
--   Actual type: [Char]

