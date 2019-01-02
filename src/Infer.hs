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

data InferResult d = Typed (TypeTree d) | Untyped (TypeError d) deriving Show
data TypeTree d = TypeTree (T.Type TVarId d) [TypeTree d] deriving Show
data TypeError d = TypeError
    { _errorMsg :: String
    , _childResults :: [InferResult d]
    } deriving Show
type ErrorMsg = String
type TypeEnv d = Map.Map E.VarName (T.Type TVarId d)
type Infer = State NextTVarId
type NextTVarId = TVarId
type TVarId = Int
data IntermediateTree d = TypedIntermediate (TypedIntermediateTree d) | UntypedIntermediate ErrorMsg [IntermediateTree d]
data TypedIntermediateTree d = TypedIntermediateTree
    { _rootType :: T.Type TVarId d
    , _rootConstraints :: [Constraint d]
    , _children :: [TypedIntermediateTree d]
    }
data Constraint d = Constraint
    { _constraintErrorMsg :: String -- if the types cannot be unified
    , _type1 :: T.Type TVarId d
    , _type2 :: T.Type TVarId d
    }
type Substitution d = Map.Map TVarId (T.Type TVarId d)
data InfiniteType = InfiniteType

makePrisms ''InferResult
makeLenses ''TypeError
makePrisms ''IntermediateTree
makeLenses ''TypedIntermediateTree
makeLenses ''Constraint

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
                    TypedIntermediate $ TypedIntermediateTree tv constraints [] where
                        constraints = Constraint "Definition doesn't typecheck" tv t
                            : (set constraintErrorMsg "Definition doesn't typecheck" <$> getConstraints typedTree)
                _ -> UntypedIntermediate "Definition doesn't typecheck" []
        Just (Left tv) -> return $ TypedIntermediate $ TypedIntermediateTree tv [] []
        Nothing -> return $ UntypedIntermediate "Definition doesn't exist" []
    E.Var var -> return $ case Map.lookup var env of
        Just t -> TypedIntermediate $ TypedIntermediateTree t [] []
        Nothing -> UntypedIntermediate "Variable isn't defined" []
    E.Fn alternatives -> do
        altTreeTuples <- traverse (inferAlternative instantiateConstructorType defs env) alternatives
        return $ case traverse getTypedTreeTuple altTreeTuples of
            Just typedAltTreeTuples -> TypedIntermediate $
                TypedIntermediateTree fnType constraints typedTrees where
                    fnType = T.fn firstPatternType firstExprType
                    (firstPatternType NonEmpty.:| restOfPatternTypes) = view (_1 . rootType) <$> typedAltTreeTuples
                    (firstExprType NonEmpty.:| restOfExprTypes) = view (_2 . rootType) <$> typedAltTreeTuples
                    constraints = patternConstraints ++ exprConstraints
                    patternConstraints = zipWith createPatternConstraint [2..] restOfPatternTypes
                    createPatternConstraint i t =
                        Constraint ("Type of pattern " ++ show i ++ " doesn't match previous ones") t firstPatternType
                    exprConstraints = zipWith createExprConstraint [2..] restOfExprTypes
                    createExprConstraint i t =
                        Constraint ("Type of expression " ++ show i ++ " doesn't match previous ones") t firstExprType
                    typedTrees = NonEmpty.toList typedAltTreeTuples >>= \(patternTree, exprTree) -> [patternTree, exprTree]
            Nothing -> UntypedIntermediate "Child doesn't typecheck" trees where
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
                let constraint = Constraint "Call doesn't typecheck" calleeType (T.fn argType resultType)
                let tree = TypedIntermediateTree resultType [constraint] [typedCalleeTree, typedArgTree]
                return $ TypedIntermediate tree
            _ -> return $ UntypedIntermediate "Child doesn't typecheck" [calleeTree, argTree]
    E.Constructor key -> case instantiateConstructorType key of
        Just getType -> do
            t <- getType
            return $ TypedIntermediate $ TypedIntermediateTree t [] []
        Nothing -> return $ UntypedIntermediate "Data constructor isn't defined" []
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
        case instantiateConstructorType key of
            Just getConstructorType -> case traverse (preview _TypedIntermediate) childTrees of
                Just typedChildTrees -> do
                    let childTypes = (\(TypedIntermediateTree t _ _) -> t) <$> typedChildTrees
                    tv <- freshTVar
                    constructorType <- getConstructorType
                    let constraint = Constraint "Args don't match data constructor's type" constructorType (foldr T.fn tv childTypes)
                    return (TypedIntermediate $ TypedIntermediateTree tv [constraint] typedChildTrees, typeEnv)
                Nothing -> return (UntypedIntermediate "Child doesn't typecheck" childTrees, typeEnv)
            Nothing -> return (UntypedIntermediate "Data constructor isn't defined" childTrees, typeEnv)
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

getConstraints :: TypedIntermediateTree d -> [Constraint d]
getConstraints (TypedIntermediateTree _ constraints children) = constraints ++ (children >>= getConstraints)

solve :: Eq d => IntermediateTree d -> InferResult d
solve intermediateTree = applyToInferResult subst unsubstitutedInferResult where
    (subst, unsubstitutedInferResult) = solve' Map.empty intermediateTree

solve' :: Eq d => Substitution d -> IntermediateTree d -> (Substitution d, InferResult d)
solve' initialSubst tree = case tree of
    TypedIntermediate (TypedIntermediateTree t constraints children) -> case traverse (preview _Typed) childInferResults of
        Just typeTrees -> (finalSubst, inferResult) where
            (finalSubst, maybeConstraintError) = solveConstraints substAfterSolvingChildren constraints
            inferResult = case maybeConstraintError of
                Just e -> Untyped $ TypeError e childInferResults
                Nothing -> Typed $ TypeTree t typeTrees
        Nothing -> (substAfterSolvingChildren, inferResult) where
            inferResult = Untyped $ TypeError "Child doesn't typecheck" childInferResults
        where (substAfterSolvingChildren, childInferResults) = solveChildren $ TypedIntermediate <$> children
    UntypedIntermediate errorMsg children -> (substAfterSolvingChildren, inferResult) where
        (substAfterSolvingChildren, childInferResults) = solveChildren children
        inferResult = Untyped $ TypeError errorMsg childInferResults
    where
        solveChildren children = (subst, childInferResults) where
            (subst, reversedChildInferResults) = foldl childrenReducer (initialSubst, []) children
            childrenReducer (s0, reversedInferResults) child = (: reversedInferResults) <$> solve' s0 child
            childInferResults = reverse reversedChildInferResults

solveConstraints :: Eq d => Substitution d -> [Constraint d] -> (Substitution d, Maybe ErrorMsg)
solveConstraints s0 constraints = case constraints of
    [] -> (s0, Nothing)
    Constraint errorMsg t1 t2 : restOfConstraints -> case unify (apply s0 t1) (apply s0 t2) of
        Just s1 -> solveConstraints (compose s1 s0) restOfConstraints
        Nothing -> (s0, Just errorMsg)

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
    Untyped (TypeError msg childResults) -> Untyped (TypeError msg $ applyToInferResult subst <$> childResults)

applyToTypeTree :: Substitution d -> TypeTree d -> TypeTree d
applyToTypeTree subst (TypeTree t children) = TypeTree (apply subst t) (applyToTypeTree subst <$> children)

hasErrorAtRoot :: TypeError d -> Bool
hasErrorAtRoot = all (is _Typed) . view childResults
