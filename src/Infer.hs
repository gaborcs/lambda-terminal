module Infer where

import Control.Exception (Exception, throw)
import Control.Monad.State
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Expr as E
import qualified Type as T

data InferResult = Typed TypeTree | TypeError TypeError deriving Show
data TypeTree = TypeTree T.Type (Map.Map E.ChildIndex TypeTree) deriving Show
type TypeError = Map.Map E.ChildIndex InferResult
type TypeEnv = Map.Map E.VarName T.Type
type Infer = State NextTVarId
type NextTVarId = T.VarId
data InferError = UnboundRef | UnboundVar deriving Show
instance Exception InferError
data InferTree = InferTree T.Type [TypeEqualityConstraint] (Map.Map E.ChildIndex InferTree)
type TypeEqualityConstraint = (T.Type, T.Type)
type Substitution = Map.Map T.VarId T.Type
newtype ErrorTree = ErrorTree (Map.Map E.ChildIndex ErrorTree)
data InfiniteType = InfiniteType

inferType :: Map.Map E.ExprName E.Expr -> E.Expr -> InferResult
inferType defs expr = createInferResult subst maybeErrorTree inferTree where
    inferTree = evalState (infer (Map.map Right defs) Map.empty expr) 0
    (subst, maybeErrorTree) = solve inferTree

infer :: Map.Map E.ExprName (Either T.Type E.Expr) -> TypeEnv -> E.Expr -> Infer InferTree
infer defs env expr = case expr of
    E.Ref ref -> case Map.lookup ref defs of
        Just (Right expr) -> do
            -- to handle recursion we create a type variable that will be used when
            -- the implementation of the expression refers to itself
            tv <- freshTVar
            inferTree@(InferTree t _ _) <- infer (Map.insert ref (Left tv) defs) env expr
            return $ InferTree tv [(tv, t)] $ Map.singleton 0 inferTree
        Just (Left tv) -> return $ InferTree tv [] Map.empty
        Nothing -> throw UnboundRef
    E.Var var -> return $ case Map.lookup var env of
        Just t -> InferTree t [] Map.empty
        Nothing -> throw UnboundVar
    E.Fn var body -> do
        paramType <- freshTVar
        bodyTree@(InferTree bodyType _ _) <- infer defs (Map.insert var paramType env) body
        return $ InferTree (T.Fn paramType bodyType) [] $ Map.singleton 0 bodyTree
    E.Call callee arg -> do
        calleeTree@(InferTree calleeType _ _) <- infer defs env callee
        argTree@(InferTree argType _ _) <- infer defs env arg
        resultType <- freshTVar
        let childTrees = Map.fromList [(0, calleeTree), (1, argTree)]
        return $ InferTree resultType [(calleeType, T.Fn argType resultType)] childTrees
    E.Int _ -> return $ InferTree T.Int [] Map.empty
    E.Plus -> return $ InferTree binaryIntOpType [] Map.empty
    E.Times -> return $ InferTree binaryIntOpType [] Map.empty

freshTVar :: Infer T.Type
freshTVar = do
    nextTVarId <- get
    put $ nextTVarId + 1
    return $ T.Var nextTVarId

binaryIntOpType :: T.Type
binaryIntOpType = T.Fn T.Int $ T.Fn T.Int T.Int

solve :: InferTree -> (Substitution, Maybe ErrorTree)
solve = solve' Map.empty

solve' :: Substitution -> InferTree -> (Substitution, Maybe ErrorTree)
solve' initialSubst (InferTree _ constraints children) = (finalSubst, maybeErrorTree) where
    (finalSubst, hasError) = foldl constraintReducer (substAfterSolvingChildren, False) constraints
    constraintReducer (s0, hasError) (t1, t2) = case unify (apply s0 t1) (apply s0 t2) of
        Just s1 -> (compose s1 s0, hasError)
        Nothing -> (s0, True)
    (substAfterSolvingChildren, childErrorTrees) = Map.foldlWithKey childrenReducer (initialSubst, Map.empty) children
    childrenReducer (s0, errorTrees) index child = maybe errorTrees (flip (Map.insert index) errorTrees) <$> solve' s0 child
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

apply :: Substitution -> T.Type -> T.Type
apply subst t = case t of
    T.Var var -> Map.findWithDefault t var subst
    T.Fn t1 t2 -> T.Fn (apply subst t1) (apply subst t2)
    T.Int -> T.Int

createInferResult :: Substitution -> Maybe ErrorTree -> InferTree -> InferResult
createInferResult subst maybeErrorTree inferTree@(InferTree t _ inferTreeChildren) = case maybeErrorTree of
    Nothing -> Typed $ createTypeTree inferTree where
        createTypeTree (InferTree t _ children) = TypeTree (apply subst t) (Map.map createTypeTree children)
    Just (ErrorTree errorChildren) -> TypeError $ Map.mapWithKey toInferResult inferTreeChildren where
        toInferResult index = createInferResult subst (Map.lookup index errorChildren)

hasErrorAtRoot :: TypeError -> Bool
hasErrorAtRoot = all isTyped

isTyped :: InferResult -> Bool
isTyped inferResult = case inferResult of
    Typed _ -> True
    _ -> False
