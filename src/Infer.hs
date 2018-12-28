{-# LANGUAGE TemplateHaskell #-}

module Infer where

import Primitive
import Control.Lens
import Control.Lens.Extras
import Control.Monad.State
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Expr as E
import qualified Pattern as P
import qualified Type as T

data InferResult d = Typed (TypeTree d) | TypeError (TypeError d) deriving Show
data TypeTree d = TypeTree (T.Type TVarId d) [TypeTree d] deriving Show
type TypeError d = [InferResult d]
type TypeEnv d = Map.Map E.VarName (T.Type TVarId d)
type Infer = State NextTVarId
type NextTVarId = TVarId
type TVarId = Int
data IntermediateTree d = TypedIntermediate (TypedIntermediateTree d) | UntypedIntermediate [IntermediateTree d]
data TypedIntermediateTree d = TypedIntermediateTree (T.Type TVarId d) [TypeEqualityConstraint d] [TypedIntermediateTree d]
type TypeEqualityConstraint d = (T.Type TVarId d, T.Type TVarId d)
type Substitution d = Map.Map TVarId (T.Type TVarId d)
data InfiniteType = InfiniteType

makePrisms ''InferResult
makePrisms ''IntermediateTree

inferType :: (Ord ed, Ord tv, Eq td)
    => (c -> Maybe (T.Type tv td))
    -> Map.Map ed (E.Expr ed c)
    -> E.Expr ed c
    -> InferResult td
inferType getConstructorType defs expr = solve intermediateTree where
    intermediateTree = evalState (infer instantiateConstructorType (Map.map Right defs) Map.empty expr) 0
    instantiateConstructorType key = instantiate <$> getConstructorType key

infer :: Ord ed
    => (c -> Maybe (Infer (T.Type TVarId td)))
    -> Map.Map ed (Either (T.Type TVarId td) (E.Expr ed c))
    -> TypeEnv td
    -> E.Expr ed c
    -> Infer (IntermediateTree td)
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
                    getAltType (TypedIntermediateTree patternType _ _, TypedIntermediateTree exprType _ _) = T.fn patternType exprType
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
                let tree = TypedIntermediateTree resultType [(calleeType, T.fn argType resultType)] [typedCalleeTree, typedArgTree]
                return $ TypedIntermediate tree
            _ -> return $ UntypedIntermediate [calleeTree, argTree]
    E.Constructor key -> case instantiateConstructorType key of
        Just getType -> do
            t <- getType
            return $ TypedIntermediate $ TypedIntermediateTree t [] []
        Nothing -> return $ UntypedIntermediate []
    E.Integer _ -> return $ TypedIntermediate $ TypedIntermediateTree T.Integer [] []
    E.Primitive p -> do
        t <- instantiate (getType p :: T.Type TVarId d)
        return $ TypedIntermediate $ TypedIntermediateTree t [] []

inferAlternative :: Ord ed
    => (c -> Maybe (Infer (T.Type TVarId td)))
    -> Map.Map ed (Either (T.Type TVarId td) (E.Expr ed c))
    -> TypeEnv td
    -> E.Alternative ed c
    -> Infer (IntermediateTree td, IntermediateTree td)
inferAlternative instantiateConstructorType defs env (patt, expr) = do
    (patternTree, patternTypeEnv) <- inferPattern instantiateConstructorType patt
    exprTree <- infer instantiateConstructorType defs (Map.union patternTypeEnv env) expr
    return (patternTree, exprTree)

inferPattern :: (c -> Maybe (Infer (T.Type TVarId d))) -> P.Pattern c -> Infer (IntermediateTree d, TypeEnv d)
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
                return (TypedIntermediate $ TypedIntermediateTree tv [(constructorType, foldr T.fn tv childTypes)] typedChildTrees, typeEnv)
            _ -> return (UntypedIntermediate childTrees, typeEnv)
    P.Integer _ -> return (TypedIntermediate $ TypedIntermediateTree T.Integer [] [], Map.empty)

freshTVar :: Infer (T.Type TVarId d)
freshTVar = do
    nextTVarId <- get
    put $ nextTVarId + 1
    return $ T.Var nextTVarId

instantiate :: Ord v => T.Type v d -> Infer (T.Type TVarId d)
instantiate t = do
    subst <- Map.fromList <$> traverse pairWithFreshTVar (T.getTypeVars t)
    return $ T.mapTypeVars (subst Map.!) t

pairWithFreshTVar :: v -> Infer (v, T.Type TVarId d)
pairWithFreshTVar var = (,) var <$> freshTVar

solve :: Eq d => IntermediateTree d -> InferResult d
solve intermediateTree = applyToInferResult subst unsubstitutedInferResult where
    (subst, unsubstitutedInferResult) = solve' Map.empty intermediateTree

solve' :: Eq d => Substitution d -> IntermediateTree d -> (Substitution d, InferResult d)
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

compose :: Substitution d -> Substitution d -> Substitution d
compose s1 s2 = Map.map (apply s1) s2 `Map.union` s1

unify :: Eq d => T.Type TVarId d -> T.Type TVarId d -> Maybe (Substitution d)
unify t1 t2 = case (t1, t2) of
    (T.Wildcard, _) -> Just Map.empty
    (_, T.Wildcard) -> Just Map.empty
    (T.Var v, t) -> either (const Nothing) Just $ bind v t
    (t, T.Var v) -> either (const Nothing) Just $ bind v t
    (T.Call a1 b1, T.Call a2 b2) -> do
        s1 <- unify a1 a2
        s2 <- unify (apply s1 b1) (apply s1 b2)
        return $ compose s2 s1
    (T.Constructor tDef1, T.Constructor tDef2) -> if tDef1 == tDef2 then Just Map.empty else Nothing
    (T.Fn, T.Fn) -> Just Map.empty
    (T.Integer, T.Integer) -> Just Map.empty
    _ -> Nothing

bind :: TVarId -> T.Type TVarId d -> Either InfiniteType (Substitution d)
bind var t = case t of
    T.Var v | v == var -> Right Map.empty
    _ -> if var `elem` T.getTypeVars t then Left InfiniteType else Right $ Map.singleton var t

apply :: Substitution d -> T.Type TVarId d -> T.Type TVarId d
apply subst = T.mapTypeVars $ \var -> Map.findWithDefault (T.Var var) var subst

applyToInferResult :: Substitution d -> InferResult d -> InferResult d
applyToInferResult subst inferResult = case inferResult of
    Typed typeTree -> Typed $ applyToTypeTree subst typeTree
    TypeError childResults -> TypeError $ applyToInferResult subst <$> childResults

applyToTypeTree :: Substitution d -> TypeTree d -> TypeTree d
applyToTypeTree subst (TypeTree t children) = TypeTree (apply subst t) (applyToTypeTree subst <$> children)

hasErrorAtRoot :: TypeError d -> Bool
hasErrorAtRoot = all $ is _Typed
