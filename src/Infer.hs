module Infer where

import Control.Exception (Exception, throw)
import Control.Monad.State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Expr as E
import qualified Pattern as P
import qualified Primitive
import qualified Type as T

data InferResult = Typed TypeTree | TypeError TypeError deriving Show
data TypeTree = TypeTree T.Type [TypeTree] deriving Show
type TypeError = [InferResult]
type TypeEnv = Map.Map E.VarName T.Type
type Infer = State NextTVarId
type NextTVarId = T.VarId
data InferError = UnboundRef | UnboundVar | UnboundConstructor deriving Show
instance Exception InferError
data InferTree = InferTree T.Type [TypeEqualityConstraint] [InferTree]
type TypeEqualityConstraint = (T.Type, T.Type)
type Substitution = Map.Map T.VarId T.Type
newtype ErrorTree = ErrorTree (Map.Map E.ChildIndex ErrorTree)
data InfiniteType = InfiniteType

inferType :: Map.Map E.ConstructorName T.Type -> Map.Map E.ExprName E.Expr -> E.Expr -> InferResult
inferType constructorTypes defs expr = createInferResult subst maybeErrorTree inferTree where
    inferTree = evalState (infer instantiateConstructorType (Map.map Right defs) Map.empty expr) 0
    instantiateConstructorType name = maybe (throw UnboundConstructor) instantiate $ Map.lookup name constructorTypes
    (subst, maybeErrorTree) = solve inferTree

infer :: (E.ConstructorName -> Infer T.Type) -> Map.Map E.ExprName (Either T.Type E.Expr) -> TypeEnv -> E.Expr -> Infer InferTree
infer instantiateConstructorType defs env expr = case expr of
    E.Ref ref -> case Map.lookup ref defs of
        Just (Right expr) -> do
            -- to handle recursion we create a type variable that will be used when
            -- the implementation of the expression refers to itself
            tv <- freshTVar
            inferTree@(InferTree t _ _) <- infer instantiateConstructorType (Map.insert ref (Left tv) defs) env expr
            return $ InferTree tv [(tv, t)] [inferTree]
        Just (Left tv) -> return $ InferTree tv [] []
        Nothing -> throw UnboundRef
    E.Var var -> return $ case Map.lookup var env of
        Just t -> InferTree t [] []
        Nothing -> throw UnboundVar
    E.Fn alternatives -> do
        altInferTreeTuples <- traverse (inferAlternative instantiateConstructorType defs env) alternatives
        let inferTrees = NonEmpty.toList altInferTreeTuples >>= \(patternInferTree, exprInferTree) -> [patternInferTree, exprInferTree]
        let altTypes = (\(InferTree patternType _ _, InferTree exprType _ _) -> T.Fn patternType exprType) <$> altInferTreeTuples
        let firstAltType NonEmpty.:| restOfAltTypes = altTypes
        return $ InferTree firstAltType ((,) firstAltType <$> restOfAltTypes) inferTrees
    E.Call callee arg -> do
        calleeTree@(InferTree calleeType _ _) <- infer instantiateConstructorType defs env callee
        argTree@(InferTree argType _ _) <- infer instantiateConstructorType defs env arg
        resultType <- freshTVar
        return $ InferTree resultType [(calleeType, T.Fn argType resultType)] [calleeTree, argTree]
    E.Constructor name -> do
        t <- instantiateConstructorType name
        return $ InferTree t [] []
    E.Int _ -> return $ InferTree T.Int [] []
    E.Primitive p -> return $ InferTree (Primitive.getType p) [] []

inferAlternative :: (E.ConstructorName -> Infer T.Type) -> Map.Map E.ExprName (Either T.Type E.Expr) -> TypeEnv -> E.Alternative -> Infer (InferTree, InferTree)
inferAlternative instantiateConstructorType defs env (pattern, expr) = do
    (patternInferTree, patternTypeEnv) <- inferPattern instantiateConstructorType pattern
    exprInferTree <- infer instantiateConstructorType defs (Map.union patternTypeEnv env) expr
    return (patternInferTree, exprInferTree)

inferPattern :: (E.ConstructorName -> Infer T.Type) -> P.Pattern -> Infer (InferTree, TypeEnv)
inferPattern instantiateConstructorType pattern = case pattern of
    P.Var var -> do
        tv <- freshTVar
        return (InferTree tv [] [], Map.singleton var tv)
    P.Constructor name children -> do
        constructorType <- instantiateConstructorType name
        childResults <- traverse (inferPattern instantiateConstructorType) children
        let childInferTrees = fst <$> childResults
        let childTypeEnvs = snd <$> childResults
        let childTypes = (\(InferTree t _ _) -> t) <$> childInferTrees
        tv <- freshTVar
        return (InferTree tv [(constructorType, foldr T.Fn tv childTypes)] childInferTrees, mconcat childTypeEnvs)

freshTVar :: Infer T.Type
freshTVar = do
    nextTVarId <- get
    put $ nextTVarId + 1
    return $ T.Var nextTVarId

instantiate :: T.Type -> Infer T.Type
instantiate t = do
    subst <- Map.fromList <$> traverse pairWithFreshTVar (typeVars t)
    return $ apply subst t

pairWithFreshTVar :: T.VarId -> Infer (T.VarId, T.Type)
pairWithFreshTVar var = (,) var <$> freshTVar

solve :: InferTree -> (Substitution, Maybe ErrorTree)
solve = solve' Map.empty

solve' :: Substitution -> InferTree -> (Substitution, Maybe ErrorTree)
solve' initialSubst (InferTree _ constraints children) = (finalSubst, maybeErrorTree) where
    (finalSubst, hasError) = foldl constraintReducer (substAfterSolvingChildren, False) constraints
    constraintReducer (s0, hasError) (t1, t2) = case unify (apply s0 t1) (apply s0 t2) of
        Just s1 -> (compose s1 s0, hasError)
        Nothing -> (s0, True)
    (substAfterSolvingChildren, childErrorTrees) = foldl childrenReducer (initialSubst, Map.empty) $ zip [0..] children
    childrenReducer (s0, errorTrees) (index, child) = maybe errorTrees (flip (Map.insert index) errorTrees) <$> solve' s0 child
    maybeErrorTree = if hasError || not (Map.null childErrorTrees) then Just (ErrorTree childErrorTrees) else Nothing

compose :: Substitution -> Substitution -> Substitution
compose s1 s2 = Map.map (apply s1) s2 `Map.union` s1

unify :: T.Type -> T.Type -> Maybe Substitution
unify t1 t2 = case (t1, t2) of
    (T.Var v, t) -> either (const Nothing) Just $ bind v t
    (t, T.Var v) -> either (const Nothing) Just $ bind v t
    (T.Fn a1 b1, T.Fn a2 b2) -> do
        s1 <- unify a1 a2
        s2 <- unify (apply s1 b1) (apply s1 b2)
        return $ compose s2 s1
    (T.Constructor name1 ts1, T.Constructor name2 ts2) -> if name1 == name2 then maybeSubst else Nothing where
        maybeSubst = foldl compose Map.empty <$> maybeSubsts
        maybeSubsts = sequence $ zipWith unify ts1 ts2
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
    T.Constructor _ ts -> foldl List.union [] $ typeVars <$> ts
    T.Int -> []

apply :: Substitution -> T.Type -> T.Type
apply subst t = case t of
    T.Var var -> Map.findWithDefault t var subst
    T.Fn t1 t2 -> T.Fn (apply subst t1) (apply subst t2)
    T.Constructor name ts -> T.Constructor name $ apply subst <$> ts
    T.Int -> T.Int

createInferResult :: Substitution -> Maybe ErrorTree -> InferTree -> InferResult
createInferResult subst maybeErrorTree inferTree@(InferTree t _ inferTreeChildren) = case maybeErrorTree of
    Nothing -> Typed $ createTypeTree inferTree where
        createTypeTree (InferTree t _ children) = TypeTree (apply subst t) (createTypeTree <$> children)
    Just (ErrorTree errorChildren) -> TypeError $ toInferResult <$> zip [0..] inferTreeChildren where
        toInferResult (index, child) = createInferResult subst (Map.lookup index errorChildren) child

hasErrorAtRoot :: TypeError -> Bool
hasErrorAtRoot = all isTyped

isTyped :: InferResult -> Bool
isTyped inferResult = case inferResult of
    Typed _ -> True
    _ -> False
