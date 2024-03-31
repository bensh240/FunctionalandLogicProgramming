-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW1.hs should successfully compile.

-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE StandaloneDeriving #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}


module HW1 where

import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Int, Maybe (..), Num (..), Ord (..), Show (..), String, all, and, any, concat, concatMap, const, curry, elem, error, filter, flip, foldl, foldr, fst, id, length, lookup, map, not, notElem, null, or, product, reverse, snd, sum, uncurry, undefined, (!!), ($), (&&), (++), (.), (||))

-- Utility
singleton :: a -> [a]
singleton = (: [])

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f .: g = \x y -> f $ g x y

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f (Just x) = Just $ f x
maybeMap _ _ = Nothing

eitherMap :: (b -> c) -> Either a b -> Either a c
eitherMap f (Right b) = Right $ f b
eitherMap _ (Left l) = Left l

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither a Nothing  = Left a
maybeToEither _ (Just b) = Right b

-- Section 1: Utility functions
-- Basic Maybes
fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just x) = x

maybe :: b -> (a -> b) -> Maybe a -> b
maybe b _ Nothing = b
maybe _ f (Just a) = f a

catMaybes :: [Maybe a] -> [a]
catMaybes = concatMap $ maybe [] singleton

-- >>> catMaybes [Just 1, Nothing , Just 3]
-- [1,3]

--   https://www.haskell.org/ghc/reportabug
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe = catMaybes .: map

-- Basic Eithers
either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left a) = f a
either _ g (Right b) = g b

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left $ f a
mapLeft _ (Right b) = Right b

catEithers :: [Either a b] -> Either a [b]
catEithers = foldr go $ Right [] where
  go (Left a) _ = Left a
  go (Right b) rest = eitherMap (b :) rest

mapEither :: (a -> Either b c) -> [a] -> Either b [c]
mapEither = catEithers .: map

concatEitherMap :: (b -> Either a c) -> Either a b -> Either a c
concatEitherMap f (Right a) = f a
concatEitherMap _ (Left b) = Left b

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr go ([], [])
 where
  go (Left a) (as, bs)  = (a : as, bs)
  go (Right b) (as, bs) = (as, b : bs)

-- Section 1: Lists and zips
-- Fun with lists and zips
-- snoc is the opposite of cons, i.e., *append* to list.
snoc :: [a] -> a -> [a]
snoc xs = (xs ++) . singleton  
-- If one list is shorter than the other, take the shorter one, e.g.,
-- zipWith (+) [1, 2] [3, 4, 5] returns [4, 6]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x : xs) (y : ys) = f x y : zipWith f xs ys
-- If one list is shorter than the other, take the shorter one, e.g.,
-- zip [1, 2] [3, 4, 5] returns [(1, 3), (2, 4)]
-- Could you implement this using zipWith and point-free style?
unzip :: [(a, b)] -> ([a], [b])
unzip = foldr go ([], []) 
  where
     go (a, b) (as, bs) = (a : as, b : bs)

zip :: [a] -> [b] -> [(a, b)]
zip = zipWith (,)

-- Section 3: String interpolation
-- Parsing template strings, e.g., "Hello ${name}!". See the PDF for more information.
splitOn :: Char -> String -> Maybe (String, String)
splitOn _ [] = Nothing
splitOn c (x : xs) | c == x = Just ([], xs)
splitOn c (x : xs) = maybeMap (\ (p, s) -> (x : p, s)) $ splitOn c xs


type Variable = String
data ParsedString = PlainString String | Variable String deriving Show
parseTemplate :: String -> Maybe [ParsedString]
parseTemplate "" = Just []
parseTemplate ('}' : _) = Nothing
parseTemplate ('{' : _) = Nothing
parseTemplate x = case splitOn '$' x of
  Nothing -> if '{' `elem` x || '}' `elem` x then Nothing else Just [PlainString x]
  Just (pre, post) -> maybeMap ([PlainString pre | pre /= ""] ++) $ case post of
    '{' : rest -> case splitOn '}' rest of
      Just (pre', post') ->
        if null pre'
          then parseTemplate post'
          else maybeMap (Variable pre': ) $ parseTemplate post'
      Nothing -> Nothing
    _ -> Nothing

type VariableName = String
type VariableValue = String
type MissingVariable = String
assignTemplate :: [(VariableName, VariableValue)] -> [ParsedString] -> Either MissingVariable String
assignTemplate vars = eitherMap concat . mapEither go
 where
  go :: ParsedString -> Either MissingVariable String
  go (PlainString s) = Right s
  go (Variable s) = maybeToEither s $ lookup s vars

data Error = MissingVar MissingVariable | InvalidTemplate deriving Show
interpolateString :: [(VariableName, VariableValue)] -> String -> Either Error String
interpolateString vars s =
  concatEitherMap
    (mapLeft MissingVar . assignTemplate vars)
    (maybeToEither InvalidTemplate $ parseTemplate s)

-- Section 4: N-queens problem
-- Queens and helpers.
-- range of a non-positive number is empty, range 3 is [0, 1, 2]
range :: Int -> [Int]
range = reverse . go
 where
  go n | n <= 0 = []
  go n = (n - 1) : go (n - 1)

-- enumerate "foo" should return [(0, 'f'), (1, 'o'), (2, 'o')]
-- Hint: Use zip
enumerate :: [a] -> [(Int, a)]
enumerate xs = zipWith (,) (range $ length xs) xs
-- Splits [1, 2, 3] should return [([1, 2, 3],[]), ([1, 2], [3]), ([1], [2, 3]), ([], [1, 2, 3]).
-- Order is important!
-- Hint: Splits [] is [([], [])].
splits :: [a] -> [([a], [a])]
splits [] = [([], [])]
splits (x : xs) = [(x : a, b) | (a, b) <- splits xs] `snoc` ([], x : xs)
-- permutations of [] is [[]]
-- permutations of [1, 2, 3] is [[1, 2, 3], [1, 3, 2], [2, 3, 1], [2, 1, 3], [3, 1, 2], [3, 2, 1]]
-- Hint: use splits
-- order is not important
permutations :: [a] -> [[a]]
permutations = foldr go [[]]
 where
  go x perms = [a ++ (x : b) | p <- perms, (a, b) <- splits p]

type Column = Int
type Solution = [Column]

-- Returns all the solutions the n-queens problem. Returns a list of solutions, each solution made
-- up up of column per row. For example, queens 1 returns [[0]], queens 2 and queens 3 return [],
-- queens 4 returns [[1,3,0,2],[2,0,3,1]].
-- Order is not important.
queens :: Int -> [Solution]
queens = map (map snd) . filter isValid . map enumerate . permutations . range
 where
  isValid :: [(Int, Int)] -> Bool
  isValid [] = True
  isValid (x : xs) = doesNotThreaten x xs && isValid xs
  doesNotThreaten :: (Int, Int) -> [(Int, Int)] -> Bool
  doesNotThreaten x xs =
    noQueenAtSameRow (fst x) (map fst xs) &&
    noQueenAtSameColumn (snd x) (map snd xs) &&
    noQueenAtDiagonalR x xs &&
    noQueenAtDiagonalL x xs
  noQueenAtSameRow = not .: elem
  noQueenAtSameColumn = not .: elem
  noQueenAtDiagonalR x xs = all (\y -> fst y - fst x /= snd y - snd x) xs
  noQueenAtDiagonalL x xs = all (\y -> fst y - fst x /= snd x - snd y) xs
