{-# LANGUAGE TemplateHaskell #-}

module Infer where

import Primitive
import Control.Lens
import Control.Lens.Extras
import Control.Monad.State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Expr as E
import qualified Pattern as P
import qualified Type as T

data InferResult t = Typed (TypeTree t) | TypeError (TypeError t) deriving Show
data TypeTree t = TypeTree (T.Type t) [TypeTree t] deriving Show
type TypeError t = [InferResult t]
type TypeEnv t = Map.Map E.VarName (T.Type t)
type Infer = State NextTVarId
type NextTVarId = T.VarId
data IntermediateTree t = TypedIntermediate (TypedIntermediateTree t) | UntypedIntermediate [IntermediateTree t]
data TypedIntermediateTree t = TypedIntermediateTree (T.Type t) [TypeEqualityConstraint t] [TypedIntermediateTree t]
type TypeEqualityConstraint t = (T.Type t, T.Type t)
type Substitution t = Map.Map T.VarId (T.Type t)
data InfiniteType = InfiniteType

makePrisms ''InferResult
makePrisms ''IntermediateTree

inferType :: (Eq t, Ord d)
    => (c -> Maybe (T.Type t))
    -> Map.Map d (E.Expr d c)
    -> E.Expr d c
    -> InferResult t
inferType getConstructorType defs expr = solve intermediateTree where
    intermediateTree = evalState (infer instantiateConstructorType (Map.map Right defs) Map.empty expr) 0
    instantiateConstructorType key = instantiate <$> getConstructorType key

infer :: Ord d
    => (c -> Maybe (Infer (T.Type t)))
    -> Map.Map d (Either (T.Type t) (E.Expr d c))
    -> TypeEnv t
    -> E.Expr d c
    -> Infer (IntermediateTree t)
infer instantiateConstructorType defs env expr = case expr of
    E.Hole -> TypedIntermediate <$> (TypedIntermediateTree <$> freshTVar <*> pure [] <*> pure [])
    E.Def defId -> case Map.lookup defId defs of
        Just (Right expr) -> do
            -- to handle recursion we create a type variable that will be used when
            -- the implementation of the expression refers to itself
            tv <- freshTVar
            tree <- infer instantiateConstructorType (Map.insert defId (Left tv) defs) env expr
            return $ case tree of
                TypedIntermediate typedTree@(TypedIntermediateTree t _ _) ->
                    TypedIntermediate $ TypedIntermediateTree tv [(tv, t)] [typedTree]
                _ -> UntypedIntermediate []
        Just (Left tv) -> return $ TypedIntermediate $ TypedIntermediateTree tv [] []
        Nothing -> return $ UntypedIntermediate []
    E.Var var -> return $ case Map.lookup var env of
        Just t -> TypedIntermediate $ TypedIntermediateTree t [] []
        Nothing -> UntypedIntermediate []
    E.Fn alternatives -> do
        altTreeTuples <- traverse (inferAlternative instantiateConstructorType defs env) alternatives
        return $ case traverse getTypedTreeTuple altTreeTuples of
            Just typedAltTreeTuples -> TypedIntermediate $
                TypedIntermediateTree firstAltType ((,) firstAltType <$> restOfAltTypes) typedTrees where
                    firstAltType NonEmpty.:| restOfAltTypes = getAltType <$> typedAltTreeTuples
                    getAltType (TypedIntermediateTree patternType _ _, TypedIntermediateTree exprType _ _) = T.Fn patternType exprType
                    typedTrees = NonEmpty.toList typedAltTreeTuples >>= \(patternTree, exprTree) -> [patternTree, exprTree]
            Nothing -> UntypedIntermediate trees where
                trees = NonEmpty.toList altTreeTuples >>= \(patternTree, exprTree) -> [patternTree, exprTree]
        where
            getTypedTreeTuple (TypedIntermediate tree1, TypedIntermediate tree2) = Just (tree1, tree2)
            getTypedTreeTuple _ = Nothing
    E.Call callee arg -> do
        calleeTree <- infer instantiateConstructorType defs env callee
        argTree <- infer instantiateConstructorType defs env arg
        case (calleeTree, argTree) of
            (TypedIntermediate typedCalleeTree, TypedIntermediate typedArgTree) -> do
                let TypedIntermediateTree calleeType _ _ = typedCalleeTree
                let TypedIntermediateTree argType _ _ = typedArgTree
                resultType <- freshTVar
                let tree = TypedIntermediateTree resultType [(calleeType, T.Fn argType resultType)] [typedCalleeTree, typedArgTree]
                return $ TypedIntermediate tree
            _ -> return $ UntypedIntermediate [calleeTree, argTree]
    E.Constructor key -> case instantiateConstructorType key of
        Just getType -> do
            t <- getType
            return $ TypedIntermediate $ TypedIntermediateTree t [] []
        Nothing -> return $ UntypedIntermediate []
    E.Int _ -> return $ TypedIntermediate $ TypedIntermediateTree T.Int [] []
    E.Primitive p -> do
        t <- instantiate $ getType p
        return $ TypedIntermediate $ TypedIntermediateTree t [] []

inferAlternative :: Ord d
    => (c -> Maybe (Infer (T.Type t)))
    -> Map.Map d (Either (T.Type t) (E.Expr d c))
    -> TypeEnv t
    -> E.Alternative d c
    -> Infer (IntermediateTree t, IntermediateTree t)
inferAlternative instantiateConstructorType defs env (patt, expr) = do
    (patternTree, patternTypeEnv) <- inferPattern instantiateConstructorType patt
    exprTree <- infer instantiateConstructorType defs (Map.union patternTypeEnv env) expr
    return (patternTree, exprTree)

inferPattern :: (c -> Maybe (Infer (T.Type t))) -> P.Pattern c -> Infer (IntermediateTree t, TypeEnv t)
inferPattern instantiateConstructorType patt = case patt of
    P.Wildcard -> do
        tv <- freshTVar
        return (TypedIntermediate $ TypedIntermediateTree tv [] [], Map.empty)
    P.Var var -> do
        tv <- freshTVar
        return (TypedIntermediate $ TypedIntermediateTree tv [] [], Map.singleton var tv)
    P.Constructor key children -> do
        childResults <- traverse (inferPattern instantiateConstructorType) children
        let childTrees = fst <$> childResults
        let childTypeEnvs = snd <$> childResults
        let typeEnv = mconcat childTypeEnvs
        case (instantiateConstructorType key, traverse (preview _TypedIntermediate) childTrees) of
            (Just getConstructorType, Just typedChildTrees) -> do
                let childTypes = (\(TypedIntermediateTree t _ _) -> t) <$> typedChildTrees
                tv <- freshTVar
                constructorType <- getConstructorType
                return (TypedIntermediate $ TypedIntermediateTree tv [(constructorType, foldr T.Fn tv childTypes)] typedChildTrees, typeEnv)
            _ -> return (UntypedIntermediate childTrees, typeEnv)
    P.Int _ -> return (TypedIntermediate $ TypedIntermediateTree T.Int [] [], Map.empty)

freshTVar :: Infer (T.Type t)
freshTVar = do
    nextTVarId <- get
    put $ nextTVarId + 1
    return $ T.Var nextTVarId

instantiate :: T.Type t -> Infer (T.Type t)
instantiate t = do
    subst <- Map.fromList <$> traverse pairWithFreshTVar (typeVars t)
    return $ apply subst t

pairWithFreshTVar :: T.VarId -> Infer (T.VarId, T.Type t)
pairWithFreshTVar var = (,) var <$> freshTVar

solve :: Eq t => IntermediateTree t -> InferResult t
solve intermediateTree = applyToInferResult subst unsubstitutedInferResult where
    (subst, unsubstitutedInferResult) = solve' Map.empty intermediateTree

solve' :: Eq t => Substitution t -> IntermediateTree t -> (Substitution t, InferResult t)
solve' initialSubst tree = (finalSubst, inferResult) where
    (maybeType, constraints, children) = case tree of
        TypedIntermediate (TypedIntermediateTree t constraints children) -> (Just t, constraints, TypedIntermediate <$> children)
        UntypedIntermediate children -> (Nothing, [], children)
    (finalSubst, hasConstraintError) = foldl constraintReducer (substAfterSolvingChildren, False) constraints
    constraintReducer (s0, hasError) (t1, t2) = case unify (apply s0 t1) (apply s0 t2) of
        Just s1 -> (compose s1 s0, hasError)
        Nothing -> (s0, True)
    (substAfterSolvingChildren, reversedChildInferResults) = foldl childrenReducer (initialSubst, []) children
    childrenReducer (s0, reversedInferResults) child = (: reversedInferResults) <$> solve' s0 child
    inferResult = case (maybeType, maybeTypeTrees) of
        (Just t, Just typeTrees) | not hasConstraintError -> Typed $ TypeTree t typeTrees
        _ -> TypeError childInferResults
    maybeTypeTrees = traverse (preview _Typed) childInferResults
    childInferResults = reverse reversedChildInferResults

compose :: Substitution t -> Substitution t -> Substitution t
compose s1 s2 = Map.map (apply s1) s2 `Map.union` s1

unify :: Eq t => T.Type t -> T.Type t -> Maybe (Substitution t)
unify t1 t2 = case (t1, t2) of
    (T.Var v, t) -> either (const Nothing) Just $ bind v t
    (t, T.Var v) -> either (const Nothing) Just $ bind v t
    (T.Fn a1 b1, T.Fn a2 b2) -> do
        s1 <- unify a1 a2
        s2 <- unify (apply s1 b1) (apply s1 b2)
        return $ compose s2 s1
    (T.Constructor tDef1 ts1, T.Constructor tDef2 ts2) -> if tDef1 == tDef2 then maybeSubst else Nothing where
        maybeSubst = foldl compose Map.empty <$> maybeSubsts
        maybeSubsts = zipWithM unify ts1 ts2
    (T.Int, T.Int) -> Just Map.empty
    _ -> Nothing

bind :: T.VarId -> T.Type t -> Either InfiniteType (Substitution t)
bind var t = case t of
    T.Var v | v == var -> Right Map.empty
    _ -> if var `elem` typeVars t then Left InfiniteType else Right $ Map.singleton var t

typeVars :: T.Type t -> [T.VarId]
typeVars t = case t of
    T.Var var -> [var]
    T.Fn t1 t2 -> typeVars t1 `List.union` typeVars t2
    T.Constructor _ ts -> foldl List.union [] $ typeVars <$> ts
    T.Int -> []

apply :: Substitution t -> T.Type t -> T.Type t
apply subst t = case t of
    T.Var var -> Map.findWithDefault t var subst
    T.Fn t1 t2 -> T.Fn (apply subst t1) (apply subst t2)
    T.Constructor name ts -> T.Constructor name $ apply subst <$> ts
    T.Int -> T.Int

applyToInferResult :: Substitution t -> InferResult t -> InferResult t
applyToInferResult subst inferResult = case inferResult of
    Typed typeTree -> Typed $ applyToTypeTree subst typeTree
    TypeError childResults -> TypeError $ applyToInferResult subst <$> childResults

applyToTypeTree :: Substitution t -> TypeTree t -> TypeTree t
applyToTypeTree subst (TypeTree t children) = TypeTree (apply subst t) (applyToTypeTree subst <$> children)

hasErrorAtRoot :: TypeError t -> Bool
hasErrorAtRoot = all $ is _Typed
