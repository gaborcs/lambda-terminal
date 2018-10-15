{-# LANGUAGE TemplateHaskell #-}

module Infer where

import Control.Lens
import Control.Lens.Extras
import Control.Monad.State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Expr as E
import qualified Pattern as P
import qualified Type as T

data InferResult = Typed TypeTree | TypeError TypeError deriving Show
data TypeTree = TypeTree T.Type [TypeTree] deriving Show
type TypeError = [InferResult]
type TypeEnv = Map.Map E.VarName T.Type
type Infer = State NextTVarId
type NextTVarId = T.VarId
data IntermediateTree = TypedIntermediate TypedIntermediateTree | UntypedIntermediate [IntermediateTree]
data TypedIntermediateTree = TypedIntermediateTree T.Type [TypeEqualityConstraint] [TypedIntermediateTree]
type TypeEqualityConstraint = (T.Type, T.Type)
type Substitution = Map.Map T.VarId T.Type
data InfiniteType = InfiniteType

makePrisms ''InferResult
makePrisms ''IntermediateTree

inferType :: (Ord d, E.Primitive p)
    => Map.Map E.ConstructorName T.Type
    -> Map.Map d (E.Expr d p)
    -> E.Expr d p
    -> InferResult
inferType constructorTypes defs expr = solve intermediateTree where
    intermediateTree = evalState (infer instantiateConstructorType (Map.map Right defs) Map.empty expr) 0
    instantiateConstructorType name = instantiate <$> Map.lookup name constructorTypes

infer :: (Ord d, E.Primitive p)
    => (E.ConstructorName -> Maybe (Infer T.Type))
    -> Map.Map d (Either T.Type (E.Expr d p))
    -> TypeEnv
    -> E.Expr d p
    -> Infer IntermediateTree
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
    E.Constructor name -> do
        case instantiateConstructorType name of
            Just getType -> do
                t <- getType
                return $ TypedIntermediate $ TypedIntermediateTree t [] []
            Nothing -> return $ UntypedIntermediate []
    E.Int _ -> return $ TypedIntermediate $ TypedIntermediateTree T.Int [] []
    E.Primitive p -> return $ TypedIntermediate $ TypedIntermediateTree (E.getType p) [] []

inferAlternative :: (Ord d, E.Primitive p)
    => (E.ConstructorName -> Maybe (Infer T.Type))
    -> Map.Map d (Either T.Type (E.Expr d p))
    -> TypeEnv
    -> E.Alternative d p
    -> Infer (IntermediateTree, IntermediateTree)
inferAlternative instantiateConstructorType defs env (pattern, expr) = do
    (patternTree, patternTypeEnv) <- inferPattern instantiateConstructorType pattern
    exprTree <- infer instantiateConstructorType defs (Map.union patternTypeEnv env) expr
    return (patternTree, exprTree)

inferPattern :: (E.ConstructorName -> Maybe (Infer T.Type)) -> P.Pattern -> Infer (IntermediateTree, TypeEnv)
inferPattern instantiateConstructorType pattern = case pattern of
    P.Wildcard -> do
        tv <- freshTVar
        return (TypedIntermediate $ TypedIntermediateTree tv [] [], Map.empty)
    P.Var var -> do
        tv <- freshTVar
        return (TypedIntermediate $ TypedIntermediateTree tv [] [], Map.singleton var tv)
    P.Constructor name children -> do
        childResults <- traverse (inferPattern instantiateConstructorType) children
        let childTrees = fst <$> childResults
        let childTypeEnvs = snd <$> childResults
        let typeEnv = mconcat childTypeEnvs
        case (instantiateConstructorType name, traverse (preview _TypedIntermediate) childTrees) of
            (Just getConstructorType, Just typedChildTrees) -> do
                let childTypes = (\(TypedIntermediateTree t _ _) -> t) <$> typedChildTrees
                tv <- freshTVar
                constructorType <- getConstructorType
                return (TypedIntermediate $ TypedIntermediateTree tv [(constructorType, foldr T.Fn tv childTypes)] typedChildTrees, typeEnv)
            _ -> return (UntypedIntermediate childTrees, typeEnv)
    P.Int _ -> return (TypedIntermediate $ TypedIntermediateTree T.Int [] [], Map.empty)

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

solve :: IntermediateTree -> InferResult
solve intermediateTree = applyToInferResult subst unsubstitutedInferResult where
    (subst, unsubstitutedInferResult) = solve' Map.empty intermediateTree

solve' :: Substitution -> IntermediateTree -> (Substitution, InferResult)
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

applyToInferResult :: Substitution -> InferResult -> InferResult
applyToInferResult subst inferResult = case inferResult of
    Typed typeTree -> Typed $ applyToTypeTree subst typeTree
    TypeError childResults -> TypeError $ applyToInferResult subst <$> childResults

applyToTypeTree :: Substitution -> TypeTree -> TypeTree
applyToTypeTree subst (TypeTree t children) = TypeTree (apply subst t) (applyToTypeTree subst <$> children)

hasErrorAtRoot :: TypeError -> Bool
hasErrorAtRoot = all $ is _Typed
