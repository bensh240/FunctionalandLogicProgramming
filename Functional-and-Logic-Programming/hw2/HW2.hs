{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW2.hs should successfully compile.

-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW2 where

import Data.Either (either, fromLeft, fromRight, isLeft, isRight, lefts, partitionEithers, rights)
import Data.List (find, foldl', uncons)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybe)
import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Int, Maybe (..), Num (..), Ord (..), Show (..), String, all, and, any, concat, concatMap, const, curry, div, drop, dropWhile, elem, error, filter, flip, foldl, foldr, fst, id, init, last, length, lookup, map, maximum, minimum, mod, not, notElem, null, or, otherwise, product, reverse, snd, sum, tail, take, takeWhile, uncurry, undefined, unzip, zip, zipWith, (!!), ($), (&&), (++), (.), (||))


-- Section 1: String matching
isPrefixOf :: String -> String -> Bool
isPrefixOf "" _ = True
isPrefixOf _ "" = False
isPrefixOf (a: as) (b: bs) = (a == b) && isPrefixOf as bs
isInfixOf :: String -> String -> Bool
isInfixOf _ "" = False
isInfixOf "" _ = True
isInfixOf (a : as) (b : bs) = isPrefixOf (a : as) (b : bs) || isInfixOf (a : as) bs
isSuffixOf :: String -> String -> Bool
isSuffixOf s1 s2 = reverse s1 `isPrefixOf` reverse s2
isSubseuenceOf :: String -> String -> Bool
isSubseuenceOf "" _ = True
isSubseuenceOf _ "" = False
isSubseuenceOf (a : as) (b : bs) = if a == b then isSubseuenceOf as bs else isSubseuenceOf (a : as) bs



-- Section 2: Document searching
type Phrase = String
data Query = All [Query] | Any [Query] | None [Query] | Literal Phrase deriving Show
type Document = String
singleDocumentCheck :: Query -> Document -> Bool
singleDocumentCheck q d = case q of
    Literal x ->  x `isInfixOf` d
    Any (x : xs) -> singleDocumentCheck x d || singleDocumentCheck (Any xs) d
    All (x : xs) -> singleDocumentCheck x d && singleDocumentCheck (All xs) d
    None (x : xs) -> not (singleDocumentCheck x d) && singleDocumentCheck (None xs) d
    Any [] -> False
    All [] -> True
    None [] -> True

findDocuments :: Query -> [Document] -> ([Document], [Document])
findDocuments q d = findDocumentsHelper q d ([],[]) 
    where 
        findDocumentsHelper ::  Query -> [Document] -> ([Document], [Document]) -> ([Document], [Document])
        findDocumentsHelper _ [] t = t
        findDocumentsHelper q' (d' : ds) (x, y) = if singleDocumentCheck q' d' then 
            findDocumentsHelper q' ds (x ++ [d'], y)
            else findDocumentsHelper q' ds (x, y ++ [d'])


-- Section 3: InfiniteList
data InfiniteList a = a :> InfiniteList a
infixr 3 :>

itoList :: InfiniteList a -> [a]
itoList (x :> xs) = x : itoList xs

irepeat :: a -> InfiniteList a
irepeat a = a :> irepeat a
iiterate :: (a -> a) -> a -> InfiniteList a
iiterate f a = f a :> iiterate f  (f a)
icycle :: [a] -> InfiniteList a
icycle l = go l l
    where
        go :: [a] -> [a] -> InfiniteList a
        go [] l' = go l' l'
        go (a : as) l' = a :> go as l'

naturals :: InfiniteList Int
naturals = iiterate (+1) (-1)
integers :: InfiniteList Int
integers = 0 :> iiterate go 0
    where
        go :: Int -> Int
        go x
            | x < 0 = -x + 1
            | x > 0 = -x
            | otherwise = 1

imap :: (a -> b) -> InfiniteList a -> InfiniteList b
imap f (x :> xs) = f x :> imap f xs
iscan :: (a -> b -> b) -> b -> InfiniteList a -> InfiniteList b
iscan f x (a :> as) = f a x :> iscan f (f a x) as 

izip :: InfiniteList a -> InfiniteList b -> InfiniteList (a, b)
izip (a :> as) (b :> bs) = (a, b) :> izip as bs
interleave :: InfiniteList a -> InfiniteList a -> InfiniteList a
interleave (a :> as) (b :> bs) = a :> b :> interleave as bs

-- snoc from hw1
snoc :: [a] -> a -> [a]
snoc l a = l ++ [a]

iinits :: InfiniteList a -> InfiniteList [a]
iinits l = [] :> iscan (:) [] l

itails :: InfiniteList a -> InfiniteList (InfiniteList a)
itails (_ :> xs) = xs :> itails xs
-- Bonus: if you don't wish to implement this, simply write ifind = undefined
ifind :: forall a. (a -> Bool) -> InfiniteList (InfiniteList a) -> Bool
ifind = undefined

-- Section 4: Binary trees (no necessarily search trees)
data Tree a = EmptyTree | Tree (Tree a) a (Tree a) deriving Show
preOrder :: Tree a -> [a]
preOrder EmptyTree = []
preOrder (Tree t1 a t2) = a : preOrder t1 ++ preOrder t2

postOrder :: Tree a -> [a]
postOrder EmptyTree = []
postOrder (Tree t1 a t2) = postOrder t1 ++ postOrder t2 ++ [a] 
inOrder :: Tree a -> [a]
inOrder EmptyTree = []
inOrder (Tree t1 a t2) = inOrder t1 ++ [a] ++ inOrder t2
levelOrder :: Tree a -> [a]
levelOrder t = go [t] 
    where 
        go :: [Tree a] -> [a]
        go [] = []
        go (EmptyTree : xs) = go xs
        go ((Tree t1 a t2) : xs) = a : go (xs ++ [t1] ++ [t2])
fromListLevelOrder :: [a] -> Tree a
fromListLevelOrder l = go 0 (zip (itoList naturals) l)
    where
        go :: Int -> [(Int,a)] -> Tree a
        go i xs = case lookup i xs of
            Nothing -> EmptyTree
            Just x -> Tree (go (2*i + 1) xs) x (go (2*i + 2) xs)


-- >>> foldr ((++) . pure) "f" "abcde"
-- Variable not in scope: pure :: Char -> [Char]
