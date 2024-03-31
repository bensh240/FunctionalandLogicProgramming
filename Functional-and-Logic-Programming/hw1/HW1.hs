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

-- Section 1: Utility functions
-- Basic Maybes
fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just a) = a

maybe :: b -> (a -> b) -> Maybe a -> b
maybe b _ Nothing = b 
maybe _ f (Just a) = f a

catMaybes :: [Maybe a] -> [a]
catMaybes l = case l of
    [] -> []
    (Nothing: xs) -> catMaybes xs 
    (Just x : xs) -> x : catMaybes xs

newL :: (a -> Maybe b) -> [a] -> [Maybe b]
newL f l = case l of
    [] -> []
    (x: xs) -> f x : newL f xs

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f l = catMaybes (newL f l)
mapMaybe f list = catMaybes [f x | x <- list]


        
-- Basic Eithers
either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left a) = f a
either _ g (Right b) = g b

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left $ f a
mapLeft _ (Right b) = Right b

catEithers :: [Either a b] -> Either a [b]
catEithers [] = Right []
catEithers (x: xs) = case x of 
    Left a -> Left a
    Right b -> case catEithers xs of
        Left a -> Left a
        Right c -> Right (b: c)


mapEither :: (a -> Either b c) -> [a] -> Either b [c]
mapEither f l = catEithers [f x | x <- l]
concatEitherMap :: (b -> Either a c) -> Either a b -> Either a c
concatEitherMap _ (Left a) = Left a
concatEitherMap f (Right b) = f b
partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers l = (chooseLeft l, chooseRight l)
    where 
        chooseLeft :: [Either a b] -> [a]
        chooseLeft [] = []
        chooseLeft (Left a: xs) =  a : chooseLeft xs 
        chooseLeft (Right b: xs) = chooseLeft xs
        chooseRight :: [Either a b] -> [b]
        chooseRight [] = []
        chooseRight (Left a: xs) =  chooseRight xs 
        chooseRight (Right b: xs) = b : chooseRight xs


-- Section 2: Lists and zips
-- Fun with lists and zips
-- snoc is the opposite of cons, i.e., append to list.
snoc :: [a] -> a -> [a]
snoc l a = l ++ [a]
-- If one list is shorter than the other, take the shorter one, e.g.,
-- zipWith (+) [1, 2] [3, 4, 5] returns [4, 6]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ _ [] = []
zipWith _ [] _ = []
zipWith f (x1 : xs1) (x2 : xs2) = f x1 x2 : zipWith f xs1 xs2
-- If one list is shorter than the other, take the shorter one, e.g.,
-- zip [1, 2] [3, 4, 5] returns [(1, 3), (2, 4)]
-- Could you implement this using zipWith and point-free style?
unzip :: [(a, b)] -> ([a], [b])
unzip l = wrapper l ([], [])
    where 
        wrapper :: [(a,b)] -> ([a], [b]) -> ([a], [b])
        wrapper [] t = t
        wrapper ((a,b) : xs) (l1, l2)= wrapper xs (snoc l1 a, snoc l2 b)
zip :: [a] -> [b] -> [(a, b)]
zip = zipWith \ a b -> (a, b)


-- Section 3: String interpolation
-- Parsing template strings, e.g., "Hello ${name}!". See the PDF for more information.
splitOn :: Char -> String -> Maybe (String, String)
splitOn c s = case s of
    [] -> Nothing
    (x: xs) -> if x == c then Just ([], xs) else case splitOn c xs of
        Nothing -> Nothing
        Just (l1, l2) -> Just (x : l1, l2)





type Variable = String
data ParsedString = PlainString String | Variable String deriving Show

parseTemplate :: String -> Maybe [ParsedString]
parseTemplate input = processInput input []
  where
    processInput :: String -> [ParsedString] -> Maybe [ParsedString]
    processInput "" acc = Just $ reverse acc
    processInput ('$':restInput) acc = case parseVariable restInput of
        Just (varName, remainingInput) -> processInput remainingInput (Variable varName : acc)
        Nothing -> Nothing
    processInput (char:restInput) acc = case acc of
        (PlainString plainStr : remainingParsed) -> processInput restInput (PlainString (plainStr ++ [char]) : remainingParsed)
        _ -> processInput restInput (PlainString [char] : acc)
    parseVariable :: String -> Maybe (String, String)
    parseVariable ('{':restInput) = parseVarContent restInput ""
        where
        parseVarContent [] _ = Nothing
        parseVarContent (c:rest) varName
            | c == '}' = Just (varName, rest)
            | otherwise = parseVarContent rest (varName ++ [c])
    parseVariable _ = Nothing



-- >>> parseTemplate " Hello${world}!"
-- Just [PlainString " Hello",Variable "world",PlainString "!"]

-- Nothing

 
type VariableName = String
type VariableValue = String
type MissingVariable = String
assignTemplate :: [(VariableName, VariableValue)] -> [ParsedString] -> Either MissingVariable String
assignTemplate vars parsedStrings = case mapEither helpFunc parsedStrings of
    Left missingVar -> Left missingVar
    Right strings -> Right (concat strings)
  where
    helpFunc :: ParsedString -> Either MissingVariable String
    helpFunc (PlainString str) = Right str
    helpFunc (Variable varName) = case lookup varName vars of
        Nothing -> Left varName
        Just varValue -> Right varValue



data Error = MissingVar MissingVariable | InvalidTemplate deriving Show
interpolateString :: [(VariableName, VariableValue)] -> String -> Either Error String
interpolateString vars template = case parseTemplate template of
    Nothing -> Left InvalidTemplate
    Just parsedTemplate -> case assignTemplate vars parsedTemplate of
        Left missingVar -> Left (MissingVar missingVar)
        Right interpolatedStr -> Right interpolatedStr

-- >>> interpolateString [(" Name ", " Simon ")] " Hello ${ name }!"
-- Left (MissingVar " name ")





-- Section 4: N-queens problem
-- Queens and helpers.
-- range of a non-positive number is empty, range 3 is [0, 1, 2]
range :: Int -> [Int]
range n = range' n 0
  where
     range' :: Int -> Int -> [Int]
     range' n i = if i == n then [] else i : range' n (i+1)
-- enumerate "foo" should return [(0, 'f'), (1, 'o'), (2, 'o')]
-- Hint: Use zip
enumerate :: [a] -> [(Int, a)]
enumerate l = zip (range $ length l) l

-- >>> enumerate "foo"
-- [(0,'f'),(1,'o'),(2,'o')]
splits :: [a] -> [([a], [a])]
splits l = go [] l
    where
        go :: [a] -> [a] -> [([a], [a])]
        go l [] = [(l, [])]
        go l (x:xs) = (l, x:xs) : go (l ++ [x]) xs
-- permutations of [] is [[]]
-- permutations of [1, 2, 3] is [[1, 2, 3], [1, 3, 2], [2, 3, 1], [2, 1, 3], [3, 1, 2], [3, 2, 1]]
-- Hint: use splits
-- order is not important
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = [l1 ++ [x] ++ l2 | perm <- permutations xs, (l1, l2) <- splits perm]
type Column = Int
type Solution = [Column]
-- Returns all the solutions the n-queens problem. Returns a list of solutions, each solution made
-- up up of column per row. For example, queens 1 returns [[0]], queens 2 and queens 3 return [],
-- queens 4 returns [[1,3,0,2],[2,0,3,1]].
-- Order is not important.
queens :: Int -> [Solution]
queens n = filter checkDiagonals l
    where 
        l = permutations . range $ n
        checkDiagonals :: [Int] -> Bool
        checkDiagonals l' = checkDiagonals' (enumerate l')
            where 
                checkDiagonals' :: [(Int, Int)] -> Bool
                checkDiagonals' [] = True
                checkDiagonals' ((x,y) : rest) = checkDiagonals' rest && checkDiagonals'' rest x y
                checkDiagonals'' :: [(Int, Int)] -> Int -> Int -> Bool
                checkDiagonals'' [] _ _ = True
                checkDiagonals'' ((a, b): rest') x y = (x - y /= a - b) && (x + y /= a + b) && checkDiagonals'' rest' x y
