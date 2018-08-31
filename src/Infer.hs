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

inferType :: Map.Map E.ExprName E.Expr -> E.Expr -> E.Path -> (Maybe ErrorPath, Maybe T.Type)
inferType defs expr path = (maybeErrorPath, maybeTypeAtPath) where
    maybeErrorPath = either Just (const Nothing) eitherErrorOrSolution
    (eitherErrorOrSolution, maybeTypeAtPath) = evalState (inferType' (Map.map Right defs) Map.empty expr path) firstTVarId
    firstTVarId = 0

inferType' ::
    Map.Map E.ExprName (Either T.Type E.Expr)
    -> TypeEnv
    -> E.Expr
    -> E.Path
    -> State NextTVarId (Either ErrorPath (T.Type, Substitution), Maybe T.Type)
inferType' defs env expr path = case expr of
    E.Ref ref -> case Map.lookup ref defs of
        Just (Right expr) -> do
            tv <- freshTVar
            (eitherErrorOrSolution, _) <- inferType' (Map.insert ref (Left tv) defs) env expr []
            return $ case eitherErrorOrSolution of
                Right (t, s1) -> case unify (apply s1 tv) t of
                    Just s2 -> if null path then (solution, Just resultType) else (solution, Nothing) where
                        solution = Right (resultType, compose s2 s1)
                        resultType = apply s2 t
                    Nothing -> (Left [], Nothing)
                Left _ -> (Left [], Nothing)
        Just (Left t) -> return (Right (t, Map.empty), Nothing)
        Nothing -> return (Left [], Nothing)
    E.Var var -> return $ case Map.lookup var env of
        Just t -> (Right (t, Map.empty), Just t)
        Nothing -> (Left [], Nothing)
    E.Fn var body -> do
        paramType <- freshTVar
        (eitherErrorOrSolutionForBody, maybeTypeAtPathInBody) <- inferType' defs (Map.insert var paramType env) body pathInChild
        return $ case eitherErrorOrSolutionForBody of
            Right (bodyType, subst) -> if null path then (solution, Just fnType) else (solution, maybeTypeAtPathInBody) where
                solution = Right (fnType, subst)
                fnType = T.Fn (apply subst paramType) bodyType
            Left errorPath -> (Left $ 0 : errorPath, Nothing)
    E.Call callee arg -> do
        (eitherErrorOrSolutionForCallee, maybeTypeAtPathInCallee) <- inferType' defs env callee pathInChild
        case eitherErrorOrSolutionForCallee of
            Right (calleeType, calleeSubst) -> do
                (eitherErrorOrSolutionForArg, maybeTypeAtPathInArg) <- inferType' defs (Map.map (apply calleeSubst) env) arg pathInChild
                (eitherErrorOrSolution, maybeResultType) <- case eitherErrorOrSolutionForArg of
                    Right (argType, argSubst) -> do
                        resultTVar <- freshTVar
                        return $ case unify (apply argSubst calleeType) (T.Fn argType resultTVar) of
                            Just unificationSubst -> (Right solution, Just substitutedResultType) where
                                solution = (substitutedResultType, composedSubst)
                                substitutedResultType = apply unificationSubst resultTVar
                                composedSubst = unificationSubst `compose` argSubst `compose` calleeSubst
                            Nothing -> (Left [], Nothing)
                    Left argErrorPath -> return (Left errorPath, Nothing) where
                        errorPath = 1 : argErrorPath
                let maybeTypeAtPath = case path of [] -> maybeResultType; 0:_ -> maybeTypeAtPathInCallee; 1:_ -> maybeTypeAtPathInArg
                return (eitherErrorOrSolution, maybeTypeAtPath)
            Left calleeErrorPath -> return (Left errorPath, maybeTypeAtPath) where
                errorPath = 0 : calleeErrorPath
                maybeTypeAtPath = case path of
                    0:_ -> maybeTypeAtPathInCallee
                    _ -> Nothing
    E.Int _ -> return $ if null path then (Right solution, Just T.Int) else (Right solution, Nothing) where
        solution = (T.Int, Map.empty)
    E.Plus -> binaryIntOp
    E.Times -> binaryIntOp
    where
        pathInChild = case path of [] -> []; _:xs -> xs
        binaryIntOp = return $ if null path then (Right solution, Just t) else (Right solution, Nothing) where
            solution = (t, Map.empty)
            t = T.Fn T.Int (T.Fn T.Int T.Int)

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
    (T.Int, T.Int) -> Just Map.empty
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
