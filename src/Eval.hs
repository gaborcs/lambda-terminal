module Eval where

import Control.Monad
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Expr as E
import qualified Pattern as P
import qualified Value as V

eval :: Map.Map E.ExprName E.Expr -> E.Expr -> Maybe V.Value
eval = eval' Map.empty

eval' :: V.Env -> Map.Map E.ExprName E.Expr -> E.Expr -> Maybe V.Value
eval' env defs expr = case expr of
    E.Ref ref -> Map.lookup ref defs >>= eval' env defs
    E.Var var -> join $ Map.lookup var env
    E.Fn alternatives -> Just $ V.Fn $ evalPatternMatching (NonEmpty.toList alternatives) where
        evalPatternMatching alternatives maybeArgVal = case alternatives of
            [] -> Nothing
            (pattern, expr):alts -> case match maybeArgVal pattern of
                Just envExtension -> eval' (Map.union envExtension env) defs expr
                Nothing -> evalPatternMatching alts maybeArgVal
    E.Call callee arg -> do
        calleeVal <- eval' env defs callee
        let maybeArgVal = eval' env defs arg -- evaluate lazily
        case calleeVal of
            V.Fn f -> f maybeArgVal
            V.Constructor name values -> Just $ V.Constructor name $ values ++ [maybeArgVal]
            _ -> Nothing
    E.Constructor name -> Just $ V.Constructor name []
    E.Int n -> Just $ V.Int n
    E.Equals -> binaryIntOp $ \a b -> V.Constructor (if a == b then "True" else "False") []
    E.Plus -> binaryIntOp $ \a b -> V.Int (a + b)
    E.Minus -> binaryIntOp $ \a b -> V.Int (a - b)
    E.Times -> binaryIntOp $ \a b -> V.Int (a * b)

match :: Maybe V.Value -> P.Pattern -> Maybe (Map.Map E.ExprName (Maybe V.Value))
match maybeValue pattern = case pattern of
    P.Var var -> Just $ Map.singleton var maybeValue -- maybeValue is not evaluated (yet) in this case
    P.Constructor name2 patterns -> case maybeValue of
        Just (V.Constructor name1 values) ->
            if name1 == name2 then mconcat <$> sequence (zipWith match values patterns) else Nothing
        _ -> Nothing

binaryIntOp :: (Int -> Int -> V.Value) -> Maybe V.Value
binaryIntOp f = Just $ V.Fn $ \maybeAVal -> Just $ V.Fn $ \maybeBVal -> case (maybeAVal, maybeBVal) of
    -- once both arguments were passed to the operation, it's time to evaluate them
    (Just (V.Int a), Just (V.Int b)) -> Just $ f a b
    _ -> Nothing
