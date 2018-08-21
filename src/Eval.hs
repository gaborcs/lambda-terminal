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
    E.Fn var body -> Just $ V.Fn env var body
    E.Call callee arg -> do
        calleeVal <- eval' env defs callee
        argVal <- eval' env defs arg
        case calleeVal of
            V.Fn env var body -> eval' (Map.insert var argVal env) defs body
            _ -> Nothing
    E.Int n -> Just $ V.Int n
