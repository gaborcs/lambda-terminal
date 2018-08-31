{-# LANGUAGE LambdaCase #-}

module Eval where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Expr as E
import qualified Value as V

eval :: Map.Map E.ExprName E.Expr -> E.Expr -> Maybe V.Value
eval = eval' Map.empty

eval' :: V.Env -> Map.Map E.ExprName E.Expr -> E.Expr -> Maybe V.Value
eval' env defs expr = case expr of
    E.Ref ref -> Map.lookup ref defs >>= eval' env defs
    E.Var var -> Map.lookup var env
    E.Fn var body -> Just . V.Fn $ \argVal -> eval' (Map.insert var argVal env) defs body
    E.Call callee arg -> eval' env defs callee >>= \case
        V.Fn f -> eval' env defs arg >>= f
        _ -> Nothing
    E.Int n -> Just $ V.Int n
    E.Plus -> Just . V.Fn $ \case
        V.Int a -> Just . V.Fn $ \case
            V.Int b -> Just . V.Int $ a + b
            _ -> Nothing
        _ -> Nothing
