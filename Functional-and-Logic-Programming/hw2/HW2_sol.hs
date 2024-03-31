{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW2.hs should successfully compile.

-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW2 where

import Data.Either (either, fromLeft, fromRight, isLeft, isRight, lefts, partitionEithers, rights)
import Data.List (find, foldl', uncons)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybe)
import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Int, Maybe (..), Num (..), Ord (..), Show (..), String, all, and, any, concat, concatMap, const, curry, drop, dropWhile, elem, error, filter, flip, foldl, foldr, fst, id, init, last, length, lookup, map, maximum, minimum, not, notElem, null, or, otherwise, product, reverse, snd, sum, tail, take, takeWhile, uncurry, undefined, unzip, zip, zipWith, (!!), ($), (&&), (++), (.), (||))

-- Utility
(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.) . (.)

-- Section 1: String matching
isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

isInfixOf :: String -> String -> Bool
isInfixOf c1 [] = null c1
isInfixOf s1 s2@(_ : ys) = s1 `isPrefixOf` s2 || s1 `isInfixOf` ys

isSuffixOf :: String -> String -> Bool
isSuffixOf s1 s2 = reverse s1 `isPrefixOf` reverse s2

isSubseuenceOf :: String -> String -> Bool
isSubseuenceOf [] _ = True
isSubseuenceOf _ [] = False
isSubseuenceOf s1@(x : xs) (y : ys) = (`isSubseuenceOf` ys) $ if x == y then xs else s1


-- Section 2: Document searching
type Phrase = String
data Query = All [Query] | Any [Query] | None [Query] | Literal Phrase
type Document = String
findDocuments :: Query -> [Document] -> ([Document], [Document])
findDocuments node = partition (filterDocument node)
 where
  partition _ [] = ([], [])
  partition p (x : xs) =
    let (as, bs) = partition p xs
     in if p x then (x : as, bs) else (as, x : bs)

  filterDocument (Literal l) s = l `isInfixOf` s
  filterDocument (Any ns) s = any (`filterDocument` s) ns
  filterDocument (All ns) s = all (`filterDocument` s) ns
  filterDocument (None ns) s = not $ filterDocument (Any ns) s

-- Section 3: InfiniteList
data InfiniteList a = a :> InfiniteList a
infixr 3 :>

itoList :: InfiniteList a -> [a]
itoList (x :> xs) = x : itoList xs

irepeat :: a -> InfiniteList a
irepeat = iiterate id

iiterate :: (a -> a) -> a -> InfiniteList a
iiterate f a = f a :> iiterate f (f a)

icycle :: [a] -> InfiniteList a
icycle [] = icycle []
icycle (x : xs) = x :> icycle (xs ++ [x])

naturals :: InfiniteList Int
naturals = iiterate (+1) (-1)

integers :: InfiniteList Int
integers = 0 :> iiterate (\x -> if x > 0 then -x else -(x - 1)) 0

izip :: InfiniteList a -> InfiniteList b -> InfiniteList (a, b)
izip (x :> xs) (y :> ys) = (x, y) :> izip xs ys

interleave :: InfiniteList a -> InfiniteList a -> InfiniteList a
interleave (x :> xs) (y :> ys) = x :> y :> interleave xs ys

iscan f b (x :> xs) = let next = f x b in next :> iscan f next xs
iscan :: (a -> b -> b) -> b -> InfiniteList a -> InfiniteList b

iinits :: InfiniteList a -> InfiniteList [a]
iinits (x :> xs) = [] :> imap (x:) (iinits xs)

itails :: InfiniteList a -> InfiniteList (InfiniteList a)
itails (_ :> xs) = go xs where go il@(_ :> ys) = il :> go ys

imap :: (a -> b) -> InfiniteList a -> InfiniteList b
imap f (x :> xs) = f x :> imap f xs

-- Bonus
ifind :: forall a. (a -> Bool) -> InfiniteList (InfiniteList a) -> Bool
ifind p = go [] where
  go :: [InfiniteList a] -> InfiniteList (InfiniteList a) -> Bool
  go x lists =
    let currentInf :> nextInfs = lists
        nextX = currentInf : map tail x
     in any (p . head) x || go nextX nextInfs
  head (x :> _) = x
  tail (_ :> xs) = xs

-- Section 4: Binary trees (no necessarily search trees)
data Tree a = EmptyTree | Tree (Tree a) a (Tree a) deriving Show
preOrder :: Tree a -> [a]
preOrder = \case
  EmptyTree -> []
  Tree l a r -> a : preOrder l ++ preOrder r

postOrder :: Tree a -> [a]
postOrder = \case
  EmptyTree -> []
  Tree l a r -> postOrder l ++ postOrder r ++ [a]

inOrder :: Tree a -> [a]
inOrder = \case
  EmptyTree -> []
  Tree l a r -> inOrder l ++ a : inOrder r

levelOrder :: Tree a -> [a]
levelOrder = concat . go where
  go :: Tree a -> [[a]]
  go = \case
    EmptyTree -> [[]]
    Tree l a r -> [a] : fix (go l) (go r)
  -- Like zip, but continues going on if one side is done
  fix :: [[a]] -> [[a]] -> [[a]]
  fix [] [] = []
  fix xs [] = xs
  fix [] xs = xs
  fix (x : xs) (y : ys) = (x ++ y) : fix xs ys

fromListLevelOrder :: [a] -> Tree a
fromListLevelOrder = \case
  [] -> EmptyTree
  (x : xs) ->
    let (l, r) = partition xs
     in Tree (fromListLevelOrder l) x (fromListLevelOrder r)
  where
    partition :: [a] -> ([a], [a])
    partition = go 1 ([], [])
    go :: Int -> ([a], [a]) -> [a] -> ([a], [a])
    go _ (l, r) [] = (reverse l, reverse r)
    go n  (l, r) xs =
      let lefts = take n xs
          rights = take n $ drop n xs
       in go (n + 1) (reverse lefts ++ l, reverse rights ++ r) (drop (n * 2) xs)
