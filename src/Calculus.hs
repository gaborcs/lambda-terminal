module Calculus where

import Control.Monad
import Data.Maybe
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

evalExprs :: Map.Map Var Expr -> Map.Map Var (Maybe Val)
evalExprs exprs = evaluatedExprs where
    evaluatedExprs = Map.map (evalUnderEnv evaluatedExprs) exprs

evalUnderEnv :: Map.Map Var (Maybe Val) -> Expr -> Maybe Val
evalUnderEnv env expr = case expr of
    IntExpr n -> Just $ IntVal n
    FnExpr var body -> Just . FnVal $ \arg -> evalUnderEnv (Map.insert var (Just arg) env) body
    VarExpr var -> join $ Map.lookup var env
    Call callee arg -> case evalUnderEnv env callee of
        Just (FnVal f) -> evalUnderEnv env arg >>= f
        _ -> Nothing

inferTypes :: Map.Map Var Expr -> Map.Map Var (Either ErrorTree Type)
inferTypes exprs = types where
    types = Map.map (inferTypeUnderEnv types) exprs

inferTypeUnderEnv :: Map.Map Var (Either ErrorTree Type) -> Expr -> Either ErrorTree Type
inferTypeUnderEnv env expr = case expr of
    IntExpr _ -> Right IntType
    FnExpr var body -> FnType var <$> inferTypeUnderEnv env body
    VarExpr var -> fromMaybe (Right $ VarType var) (Map.lookup var env)
    Call callee arg -> case (inferTypeUnderEnv env callee, inferTypeUnderEnv env arg) of
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
