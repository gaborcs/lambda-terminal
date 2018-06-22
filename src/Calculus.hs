module Calculus where

import qualified Data.Map as Map

type Var = String
type FnBody = Expr
data Expr = IntExpr Int | FnExpr Var FnBody | VarExpr Var | Call Expr Expr deriving (Eq, Show)
data Type = IntType | FnType Var Type | VarType Var deriving (Eq, Show)
data Val = IntVal Int | FnVal (Val -> Maybe Val)
data ErrorTree = CalleeNotFn
    | CalleeError ErrorTree
    | ArgError ErrorTree
    | CalleeNotFnAndArgError ErrorTree
    | CalleeAndArgError ErrorTree ErrorTree
    deriving (Eq, Show)

eval :: Expr -> Maybe Val
eval = evalWithEnv Map.empty

evalWithEnv :: Map.Map Var Val -> Expr -> Maybe Val
evalWithEnv env expr = case expr of
    IntExpr n -> Just $ IntVal n
    FnExpr var body -> Just . FnVal $ \arg -> evalWithEnv (Map.insert var arg env) body
    VarExpr var -> Map.lookup var env
    Call callee arg -> case evalWithEnv env callee of
        Just (FnVal f) -> evalWithEnv env arg >>= f
        _ -> Nothing

inferType :: Expr -> Either ErrorTree Type
inferType expr = case expr of
    IntExpr _ -> Right IntType
    FnExpr var body -> FnType var <$> inferType body
    VarExpr var -> Right $ VarType var
    Call callee arg -> case (inferType callee, inferType arg) of
        (Right (FnType var bodyType), Right argType) -> Right $ substituteVarType var argType bodyType
        (Right (FnType _ _), Left argError) -> Left $ ArgError argError
        (Right _, Right _) -> Left CalleeNotFn
        (Right _, Left argError) -> Left $ CalleeNotFnAndArgError argError
        (Left calleeError, Right _) -> Left $ CalleeError calleeError
        (Left calleeError, Left argError) -> Left $ CalleeAndArgError calleeError argError

substituteVarType :: Var -> Type -> Type -> Type
substituteVarType var replacement original = case original of
    IntType -> IntType
    FnType v body -> if v == var then original else FnType v $ substituteVarType var replacement body
    VarType var -> replacement
