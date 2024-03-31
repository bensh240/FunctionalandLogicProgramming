{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Needed for instance PrettyPrint [Statement]
{-# LANGUAGE FlexibleInstances #-}

-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
-- {-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW3 where

import qualified Data.Map as M
import Data.Map (Map, (!?))
import qualified Data.Set as S
import Data.Set (Set)
import Data.Either (either, fromLeft, fromRight, isLeft, isRight, lefts, partitionEithers, rights)
import Data.List (find, foldl', uncons)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybe)
import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Int, Maybe (..), Num (..), Ord (..), Show (..), String, all, and, any, concat, concatMap, const, curry, drop, dropWhile, elem, error, filter, flip, foldl, foldr, fst, id, init, last, length, lookup, map, maximum, minimum, not, notElem, null, or, product, replicate, reverse, snd, sum, tail, take, takeWhile, uncurry, undefined, unlines, unzip, zip, zipWith, (!!), ($), (&&), (++), (.), (||))
import Data.Ord (Down(Down))

type Variable = String
data Expression = Not Expression | Or Expression Expression | And Expression Expression | Var Variable | Literal Bool deriving (Show, Eq)
data Statement =
  Return Expression |
  Block [Statement] |
  If Expression [Statement] |
  IfElse Expression [Statement] [Statement] |
  Define Variable Expression
  deriving (Show, Eq)

data Term = V Variable | L Bool deriving (Show, Eq)

-- Section 1.1: Pretty printing expressions
class PrettyPrint a where
  prettyPrint :: a -> String

instance PrettyPrint Expression where
  prettyPrint :: Expression -> String
  prettyPrint a = case a of
     Not ex -> "!" ++ prettyPrint' ex
     Or ex ex' -> prettyPrint' ex ++ " || " ++ prettyPrint' ex'
     And ex ex' -> prettyPrint' ex ++ " && " ++ prettyPrint' ex'
     Var s -> s
     Literal True -> "true"
     Literal False -> "false"
     where
        prettyPrint' :: Expression -> String
        prettyPrint' a' = case a' of
          Not ex -> "!" ++ prettyPrint' ex
          Or ex ex' -> "(" ++ prettyPrint' ex ++ " || " ++ prettyPrint' ex' ++ ")"
          And ex ex' -> "(" ++ prettyPrint' ex ++ " && " ++ prettyPrint' ex' ++ ")"
          Var s -> s
          Literal True -> "true"
          Literal False -> "false"

instance PrettyPrint Statement where
    prettyPrint :: Statement -> String
    prettyPrint s = prettyPrint' s 0
        where
            concatSpace :: Int -> String
            concatSpace 0 = ""
            concatSpace i = "  " ++ concatSpace (i-1)
            prettyPrint' :: Statement -> Int -> String
            prettyPrint' s' n = case s' of
              Return ex -> concatSpace n ++ "return " ++ prettyPrint ex
              Block states -> init $ unlines $ [concatSpace n ++ "{"] ++ [prettyPrint' state (n+1) | state <- states] ++ [concatSpace n ++ "}"]
              If ex states -> init $ unlines $ [concatSpace n ++ "if (" ++ prettyPrint ex ++ ") {"] ++ [prettyPrint' state (n+1) | state <- states] ++ [concatSpace n ++ "}"]
              IfElse ex states states' -> init $ unlines $ [concatSpace n ++ "if (" ++ prettyPrint ex ++ ") {"] ++ [prettyPrint' state (n+1) | state <- states] ++ [concatSpace n ++ "} else {"] ++ [prettyPrint' state (n+1) | state <- states'] ++ [concatSpace n ++ "}"]
              Define str ex -> concatSpace n ++ str ++ " = " ++ prettyPrint ex

instance PrettyPrint [Statement] where
    prettyPrint :: [Statement] -> String
    prettyPrint [] = ""
    prettyPrint [s] = prettyPrint s
    prettyPrint (s: ss) = prettyPrint s ++ "\n" ++ prettyPrint ss

type Scope = Map Variable Bool

simplifyExpression :: Scope -> Expression -> Expression
simplifyExpression scope ex = case ex of
  Not ex' -> negate' ex'
    where
        negate' :: Expression -> Expression
        negate' ex = case simplifyExpression scope ex of
            Literal x -> Literal (not x)
            ex' -> Not ex'
  Or ex' ex2 -> let leftSide = simplifyExpression scope ex'
                    rightSide = simplifyExpression scope ex2
                in case (leftSide, rightSide) of
                    (Literal True, _) -> Literal True
                    (_, Literal True) -> Literal True
                    _ -> Or ex' ex2
  And ex' ex2 -> let leftSide = simplifyExpression scope ex'
                     rightSide = simplifyExpression scope ex2
                in case (leftSide, rightSide) of
                    (Literal False, _) -> Literal False
                    (_, Literal False) -> Literal False
                    _ -> And ex' ex2
  Var s -> case scope !? s of
    Just x -> Literal x
    Nothing -> Var s
  Literal b -> Literal b

-- >>> simplifyExpression 

simplifyWithScope :: Scope -> [Statement] -> [Statement]
simplifyWithScope s = reverse . snd . foldl' (uncurry go) (s, []) where
  go :: Scope -> [Statement] -> Statement -> (Scope, [Statement])
  go scope statementsSoFar statement =
    let (newScope, simplified) = simplifyStatement scope statement
     in (newScope, simplified ++ statementsSoFar)


  simplifyStatement :: Scope -> Statement -> (Scope, [Statement])
  simplifyStatement scope (Return ex) = goReturn ex scope
  -- simplifyStatement scope (Block states) = goBlock states (scope, [])
  simplifyStatement scope (Block states) =  let (finalScope, simplifiedStates) =  foldl' goBlock (scope, []) states
    in (finalScope, [Block simplifiedStates])
  simplifyStatement scope (Define var ex) = goDefine scope var ex
  simplifyStatement scope (If ex states) = goIf ex states scope
  simplifyStatement scope (IfElse ex states states') = goIfElse ex states states' scope


  -- go functions
  goReturn :: Expression -> Scope -> (Scope, [Statement])
  goReturn ex scope = (scope, [Return $ simplifyExpression scope ex])
--   goBlock :: [Statement] -> (Scope, [Statement]) -> (Scope, [Statement])
--   goBlock [] t = t
--   goBlock (s : rest) (scope, states) = goBlock rest (newScope, states ++ newStates)
--       where
--           (newScope, newStates) = simplifyStatement scope s 
  goBlock :: (Scope, [Statement]) -> Statement ->  (Scope, [Statement])
  goBlock (scope, states) d@(Define var ex) = let (newScope, simplifiedState) = simplifyStatement scope d
                                                in (newScope, states ++ simplifiedState)
  goBlock (scope, states) r@(Return ex) = let (newScope, simplifiedState) = simplifyStatement scope r
                                            in (newScope, states ++ simplifiedState)
  goBlock (scope, states) state = let (newScope, simplifiedState) = simplifyStatement scope state
    in (scope, states ++ simplifiedState)

  goDefine :: Scope -> Variable -> Expression -> (Scope, [Statement])
  goDefine scope var ex = case simplifyExpression scope ex of
    Literal x -> (M.insert var x scope, [Define var (Literal x)])
    _ -> let newScope = M.delete var scope
         in (newScope, [Define var (simplifyExpression newScope ex)])

  goIf :: Expression -> [Statement] -> Scope -> (Scope, [Statement])
  goIf ex states scope = case simplifyExpression scope ex of
    Literal False -> (scope, [])
    Literal True -> simplifyStatement scope (Block states)
    Var x -> let scope' = M.insert x True scope
                 newExpression = simplifyExpression scope ex
                 ifBlock = simplifyWithScope scope' states
                in (scope', [If newExpression ifBlock])
    _ -> (scope, states)

  goIfElse :: Expression -> [Statement] -> [Statement] -> Scope -> (Scope, [Statement])
  goIfElse ex states states' scope = case simplifyExpression scope ex of
    Literal False -> simplifyStatement scope (Block states')
    Literal True -> simplifyStatement scope (Block states)
    Var x -> let scope' = M.insert x True scope
                 scope'' = M.insert x False scope
                 ifScope = simplifyWithScope scope' states
                 elseScope = simplifyWithScope scope'' states'
                 newExpression = simplifyExpression scope ex
                in (scope', [IfElse newExpression ifScope elseScope])
    _ -> (scope, states)

simplify :: [Statement] -> [Statement]
simplify = simplifyWithScope M.empty


-- Section 2.1: Basic type classes
data Tree a = Empty | Tree (Tree a) a (Tree a)
inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Tree t1 a t2) = inOrder t1 ++ [a] ++ inOrder t2
instance Eq a => Eq (Tree a) where
  (==) :: Eq a => Tree a -> Tree a -> Bool
  t1 == t2 = inOrder t1 == inOrder t2

instance Show a => Show (Tree a) where
  show :: Show a => Tree a -> String
  show tree = "{" ++ tail (init (show $ inOrder tree)) ++ "}"

instance Ord a => Ord (Tree a) where
  (<=) , (<) , (>) , (>=) :: Tree a -> Tree a -> Bool
  t1 <= t2 = inOrder t1 <= inOrder t2
  (>=) = flip (<=)
  (>) t1 t2 = not ((<=) t1 t2)
  (<) = flip (>)

-- Section 2.2: Typeclass constraints

--Eq/ Ord is based on the first argument only !
data Arg a b = Arg a b
instance Eq a => Eq ( Arg a b) where
  ( Arg a1 _) == ( Arg a2 _ ) = a1 == a2
instance Ord a => Ord ( Arg a b ) where
  ( Arg a1 _) <= ( Arg a2 _ ) = a1 <= a2


nub :: Eq a => [a] -> [a]
nub [] = []
nub (x: xs) = x : nub (filter (/=x) xs)

sort :: Ord a => [a] -> [a]
sort l = extractFromMap $ M.toList $ buildMap l M.empty
  where 
    extractFromMap :: [(a, Int)] -> [a]
    extractFromMap [] = []
    extractFromMap (x: xs) = extractSameNumber x ++ extractFromMap xs
    extractSameNumber :: (a, Int) -> [a]
    extractSameNumber (_, 0) = []
    extractSameNumber (x, i) = x : extractSameNumber (x, i-1)
    buildMap [] m = m
    buildMap (x: xs) m = buildMap xs (M.insertWith (+) x 1 m) 



sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f l =  [b |(Arg a b) <- sort [Arg (f x) x | x <- l]] 


