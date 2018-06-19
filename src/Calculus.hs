module Calculus where

import qualified Data.Map as Map

type Var = String
type FnBody = Expr
data Expr = IntExpr Int | FnExpr Var FnBody | VarExpr Var | Call Expr Expr deriving (Eq, Show)
data Type = IntType | FnType Var Type | VarType Var deriving (Eq, Show)
data Val = IntVal Int | FnVal (Val -> Maybe Val)

eval :: Expr -> Maybe Val
eval = evalWithEnv Map.empty

evalWithEnv :: Map.Map Var Val -> Expr -> Maybe Val
evalWithEnv env expr = case expr of
    IntExpr n -> Just $ IntVal n
    FnExpr var body -> Just . FnVal $ \arg -> evalWithEnv (Map.insert var arg env) body
    VarExpr var -> Map.lookup var env
    Call f arg -> case evalWithEnv env f of
        Just (FnVal f) -> evalWithEnv env arg >>= f
        _ -> Nothing

inferType :: Expr -> Maybe Type
inferType expr = case expr of
    IntExpr _ -> Just IntType
    FnExpr var body -> FnType var <$> inferType body
    VarExpr var -> Just $ VarType var
    Call f x -> case (inferType f, inferType x) of
        (Just (FnType var bodyType), Just argType) -> Just $ substituteVarType var argType bodyType
        _ -> Nothing

substituteVarType :: Var -> Type -> Type -> Type
substituteVarType var replacement original = case original of
    IntType -> IntType
    FnType v body -> if v == var then original else FnType v $ substituteVarType var replacement body
    VarType var -> replacement
