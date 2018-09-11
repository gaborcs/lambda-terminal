{-# LANGUAGE LambdaCase #-}

module Eval where

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
    E.Var var -> Map.lookup var env
    E.Fn alternatives -> Just . V.Fn . evalPatternMatching $ NonEmpty.toList alternatives where
        evalPatternMatching alternatives argVal = case alternatives of
            [] -> Nothing
            (pattern, expr):alts -> case match argVal pattern of
                Just envExtension -> eval' (env <> envExtension) defs expr
                Nothing -> evalPatternMatching alts argVal
    E.Call callee arg -> do
        calleeVal <- eval' env defs callee
        argVal <- eval' env defs arg
        case calleeVal of
            V.Fn f -> f argVal
            V.Constructor name values -> Just $ V.Constructor name $ values ++ [argVal]
            _ -> Nothing
    E.Constructor name -> Just $ V.Constructor name []
    E.Int n -> Just $ V.Int n
    E.Plus -> Just $ binaryIntOp (+)
    E.Times -> Just $ binaryIntOp (*)

match :: V.Value -> P.Pattern -> Maybe (Map.Map E.ExprName V.Value)
match value pattern = case (value, pattern) of
    (_, P.Var var) -> Just $ Map.singleton var value
    (V.Constructor name1 values, P.Constructor name2 patterns) ->
        if name1 == name2 then mconcat <$> sequence (zipWith match values patterns) else Nothing
    _ -> Nothing

binaryIntOp :: (Int -> Int -> Int) -> V.Value
binaryIntOp f = V.Fn $ \case
    V.Int a -> Just . V.Fn $ \case
        V.Int b -> Just . V.Int $ f a b
        _ -> Nothing
    _ -> Nothing
