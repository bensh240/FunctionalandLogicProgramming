module Calculator (Expression (..), Error (..), evaluate) where

import Data.Map (Map, (!?))

data Expression
    = Identifier String
    | Literal Int
    | Plus Expression Expression
    | Minus Expression Expression
    | Mult Expression Expression
    | Division Expression Expression

data Error = DivisionByZero | MissingIdentifier String

evaluate :: Map String Int -> Expression -> Either Error Int
evaluate _ (Literal x) = Right x
evaluate ids (Identifier x) =
    maybe (Left $ MissingIdentifier x) Right $ ids !? x
evaluate ids (Plus l r) =
    evaluateOp (Right .: (+)) ids l r
evaluate ids (Minus l r) =
    evaluateOp (Right .: (-)) ids l r
evaluate ids (Mult l r) =
    evaluateOp (Right .: (*)) ids l r
evaluate ids (Division l r) =
    evaluateOp divisionAux ids l r
  where
    divisionAux _ 0 = Left DivisionByZero
    divisionAux e1 e2 = Right $ e1 `div` e2

evaluateOp ::
    (Int -> Int -> Either Error Int) ->
    Map String Int ->
    Expression ->
    Expression ->
    Either Error Int
evaluateOp op ids l r = do
    e1 <- evaluate ids l
    e2 <- evaluate ids r
    e1 `op` e2

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f .: g = \x y -> f $ g x y
