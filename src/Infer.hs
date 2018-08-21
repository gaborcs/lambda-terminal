module Infer where

import Control.Monad.State
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Expr as E
import qualified Type as T

type TypeEnv = Map.Map E.VarName T.Type
type ErrorPath = E.Path
type NextTVarId = T.VarId
type Substitution = Map.Map T.VarId T.Type
data InfiniteType = InfiniteType

inferType :: Map.Map E.ExprName E.Expr -> E.Expr -> Either ErrorPath T.Type
inferType defs expr = do
    let firstTVarId = 0
    (t, _) <- evalState (inferType' (Map.map Right defs) Map.empty expr) firstTVarId
    let indexTypeVarsFromZero = apply . Map.fromList $ zip (typeVars t) (T.Var <$> [0..])
    return $ indexTypeVarsFromZero t

inferType' ::
    Map.Map E.ExprName (Either T.Type E.Expr)
    -> TypeEnv
    -> E.Expr
    -> State NextTVarId (Either ErrorPath (T.Type, Substitution))
inferType' defs env expr = case expr of
    E.Ref ref -> case Map.lookup ref defs of
        Just (Right expr) -> do
            tv <- freshTVar
            inferResult <- inferType' (Map.insert ref (Left tv) defs) env expr
            return $ case inferResult of
                Right (t, s1) -> case unify (apply s1 tv) t of
                    Just s2 -> Right (apply s2 t, compose s2 s1)
                    Nothing -> Left []
                Left _ -> Left []
        Just (Left t) -> return $ Right (t, Map.empty)
        Nothing -> return $ Left []
    E.Var var -> return $ case Map.lookup var env of
        Just t -> Right (t, Map.empty)
        Nothing -> Left []
    E.Fn var body -> do
        paramType <- freshTVar
        bodyInferResult <- inferType' defs (Map.insert var paramType env) body
        return $ case bodyInferResult of
            Right (bodyType, subst) -> Right (T.Fn (apply subst paramType) bodyType, subst)
            Left errorPath -> Left $ 0 : errorPath
    E.Call callee arg -> do
        calleeInferResult <- inferType' defs env callee
        case calleeInferResult of
            Right (calleeType, calleeSubst) -> do
                argInferResult <- inferType' defs (Map.map (apply calleeSubst) env) arg
                case argInferResult of
                    Right (argType, argSubst) -> do
                        resultType <- freshTVar
                        return $ case unify (apply argSubst calleeType) (T.Fn argType resultType) of
                            Just unificationSubst -> Right (apply unificationSubst resultType, composedSubst) where
                                composedSubst = unificationSubst `compose` argSubst `compose` calleeSubst
                            Nothing -> Left []
                    Left errorPath -> return . Left $ 1 : errorPath
            Left errorPath -> return . Left $ 0 : errorPath
    E.Int _ -> return $ Right (T.Int, Map.empty)

freshTVar :: State NextTVarId T.Type
freshTVar = do
    nextTVarId <- get
    put $ nextTVarId + 1
    return $ T.Var nextTVarId

unify :: T.Type -> T.Type -> Maybe Substitution
unify t1 t2 = case (t1, t2) of
    (T.Var v, t) -> either (const Nothing) Just $ bind v t
    (t, T.Var v) -> either (const Nothing) Just $ bind v t
    (T.Fn a1 b1, T.Fn a2 b2) -> do
        s1 <- unify a1 a2
        s2 <- unify (apply s1 b1) (apply s1 b2)
        return $ compose s1 s2
    _ -> Nothing

bind :: T.VarId -> T.Type -> Either InfiniteType Substitution
bind var t
    | t == T.Var var = Right Map.empty
    | var `elem` typeVars t = Left InfiniteType
    | otherwise = Right $ Map.singleton var t

typeVars :: T.Type -> [T.VarId]
typeVars t = case t of
    T.Var var -> [var]
    T.Fn t1 t2 -> typeVars t1 `List.union` typeVars t2
    T.Int -> []

compose :: Substitution -> Substitution -> Substitution
compose s1 s2 = Map.map (apply s1) s2 `Map.union` s1

apply :: Substitution -> T.Type -> T.Type
apply subst t = case t of
    T.Var var -> Map.findWithDefault t var subst
    T.Fn t1 t2 -> T.Fn (apply subst t1) (apply subst t2)
    T.Int -> T.Int
