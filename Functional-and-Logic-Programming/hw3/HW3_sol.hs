{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Needed for instance PrettyPrint [Statement]
{-# LANGUAGE FlexibleInstances #-}

-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
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
import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Int, Maybe (..), Num (..), Ord (..), Ordering(..), Show (..), String, all, and, any, concat, concatMap, const, curry, drop, dropWhile, elem, error, filter, flip, foldl, foldr, fst, id, init, last, length, lookup, map, maximum, minimum, not, notElem, null, or, product, replicate, reverse, snd, sum, tail, take, takeWhile, uncurry, undefined, unlines, unzip, zip, zipWith, (!!), ($), (&&), (++), (.), (||))

type Variable = String
data Expression =
  Not Expression |
  Or Expression Expression |
  And Expression Expression |
  Var Variable |
  Literal Bool
  deriving (Show, Eq)
data Statement =
  Return Expression |
  Block [Statement] |
  If Expression [Statement] |
  IfElse Expression [Statement] [Statement] |
  Define Variable Expression
  deriving (Show, Eq)

-- Section 1
class PrettyPrint a where
  prettyPrint :: a -> String
instance PrettyPrint Expression where
  prettyPrint = \case
    Not expr -> "!" ++ go expr
    Or expr1 expr2 -> binary "||" expr1 expr2
    And expr1 expr2 -> binary "&&" expr1 expr2
    Var s -> s
    Literal b -> show b
    where
      binary op expr1 expr2 = go expr1 ++ " " ++ op ++ " " ++ go expr2
      isSimple = \case
        Var _ -> True
        Literal _ -> True
        Not _ -> True
        _ -> False
      go e =
        let pp = prettyPrint e
         in if isSimple e then pp else "(" ++ pp ++ ")"

instance PrettyPrint Statement where
  prettyPrint = unlines . go 0 where
    go :: Int -> Statement -> [String]
    go indent =
      let prefix = replicate (2 * indent) ' '
          goNext = go $ indent + 1
      in \case
        Return expr -> [prefix ++ "return " ++ prettyPrint expr]
        If cond block ->
          let if' = prefix ++ "if (" ++ prettyPrint cond ++ ") {"
              block' = concatMap goNext block
           in if' : block' ++ [prefix ++ "}"]
        IfElse cond ifBlock elseBlock ->
          let if' = prefix ++ "if (" ++ prettyPrint cond ++ ") {"
              ifBlock' = concatMap goNext ifBlock
              else' = prefix ++ "} else {"
              elseBlock' = concatMap goNext elseBlock
           in if' : ifBlock' ++ else' : elseBlock' ++ [prefix ++ "}"]
        Define term expr -> [prefix ++ term ++ " = " ++ prettyPrint expr]
        Block block ->
          let block' = concatMap goNext block
           in (prefix ++ "{") : block' ++ [prefix ++ "}"]

instance PrettyPrint [Statement] where
  prettyPrint = concatMap prettyPrint

-- Section 2
type Scope = Map Variable Bool
simplifyExpression :: Scope -> Expression -> Expression

simplifyReturn :: Scope -> Expression -> Expression
simplifyDefine :: Scope -> Expression -> Expression
simplifyIf :: Scope -> Expression -> [Statement] -> [Statement]
simplifyIfElse :: Scope -> Expression -> [Statement] -> [Statement] -> [Statement]

simplifyWithScope :: Scope -> [Statement] -> [Statement]
simplifyWithScope s = reverse . snd . foldl' (uncurry go) (s, []) where
  go :: Scope -> [Statement] -> Statement -> (Scope, [Statement])
  go scope statementsSoFar statement =
    let (newScope, simplified) = simplifyStatement scope statement
     in (newScope, simplified ++ statementsSoFar)
  simplifyStatement :: Scope -> Statement -> (Scope, [Statement])
  simplifyStatement scope = \case
    Return expr -> (scope, [Return $ simplifyReturn scope expr])
    If cond block -> (scope, simplifyIf scope cond block)
    IfElse cond ifBlock elseBlock -> (scope, simplifyIfElse scope cond ifBlock elseBlock)
    Block block -> (scope, [Block $ simplifyWithScope scope block])
    Define term expr ->
      let expr' = simplifyDefine scope expr
       in (maybeInsertExpression term expr' scope, [Define term expr'])
  maybeInsertExpression :: String -> Expression -> Scope -> Scope
  maybeInsertExpression name = \case
    Literal b -> M.insert name b
    _ -> M.delete name

simplify :: [Statement] -> [Statement]
simplify = simplifyWithScope M.empty

simplifyExpression scope = \case
  Not expr -> case simplifyExpression scope expr of
    Literal b -> Literal $ not b
    e -> Not e
  Or expr1 expr2 -> case (simplifyExpression scope expr1, simplifyExpression scope expr2) of
    (Literal True, _) -> Literal True
    (_, Literal True) -> Literal True
    (e1, e2) -> Or e1 e2
  And expr1 expr2 -> case (simplifyExpression scope expr1, simplifyExpression scope expr2) of
    (Literal False, _) -> Literal False
    (_, Literal False) -> Literal False
    (e1, e2) -> And e1 e2
  t@(Var s) -> maybe t Literal (scope !? s)
  l@Literal{} -> l

simplifyReturn = simplifyExpression
simplifyIf scope cond block =
  let scope' = case cond of
        Var s -> M.insert s True scope
        _ -> scope
      block' = simplifyWithScope scope' block
  in case simplifyExpression scope cond of
    Literal b -> [Block block' | b]
    cond' -> [If cond' block']
simplifyIfElse scope cond ifBlock elseBlock =
  let updateScope b = case cond of
        Var s -> M.insert s b scope
        _ -> scope
      ifBlock' = simplifyWithScope (updateScope True) ifBlock
      elseBlock' = simplifyWithScope (updateScope False) elseBlock
   in case simplifyExpression scope cond of
      Literal b -> [Block $ if b then ifBlock' else elseBlock']
      cond' -> [IfElse cond' ifBlock' elseBlock']
simplifyDefine = simplifyExpression

-- Section 3
data Tree a = Empty | Tree (Tree a) a (Tree a)
inOrder :: Tree a -> [a]
inOrder = \case
  Empty -> []
  Tree l a r -> inOrder l ++ [a] ++ inOrder r

instance Show a => Show (Tree a) where
  show t =
    let asListWithoutBrackets = init $ tail $ show $ inOrder t
     in "{" ++ asListWithoutBrackets ++ "}"
instance Eq a => Eq (Tree a) where
  t1 == t2 = inOrder t1 == inOrder t2
instance Ord a => Ord (Tree a) where
  t1 <= t2 = inOrder t1 <= inOrder t2

nub :: Eq a => [a] -> [a]
nub = reverse . foldl' (\xs x -> if x `elem` xs then xs else x : xs) []
sort :: Ord a => [a] -> [a]
sort = concat . M.elems . M.fromListWith (++) . map (\x -> (x, [x]))

data Arg a b = Arg a b
instance Eq a => Eq (Arg a b) where
  (Arg a1 _) == (Arg a2 _) = a1 == a2
instance Ord a => Ord (Arg a b) where
  (Arg a1 _) <= (Arg a2 _) = a1 <= a2

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = map (\(Arg _ a) -> a) . sort . map (\x -> Arg (f x) x)
