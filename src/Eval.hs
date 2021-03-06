module Eval where

import Primitive
import Control.Monad
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Expr as E
import qualified Pattern as P
import qualified Value as V

eval :: (Eq c, Ord d) => Map.Map d (E.Expr d c) -> E.Expr d c -> Maybe (V.Value c)
eval = eval' Map.empty

eval' :: (Eq c, Ord d) => V.Env c -> Map.Map d (E.Expr d c) -> E.Expr d c -> Maybe (V.Value c)
eval' env defs expr = case expr of
    E.Hole -> Nothing
    E.Def key -> Map.lookup key defs >>= eval defs
    E.Var var -> join $ Map.lookup var env
    E.Fn alts -> Just $ V.Fn $ evalPatternMatching $ NonEmpty.toList alts
    E.Call callee arg -> do
        calleeVal <- eval' env defs callee
        let maybeArgVal = eval' env defs arg -- evaluate lazily
        case calleeVal of
            V.Fn f -> f maybeArgVal
            V.Constructor key values -> Just $ V.Constructor key $ values ++ [maybeArgVal]
            _ -> Nothing
    E.Constructor key -> Just $ V.Constructor key []
    E.Integer n -> Just $ V.Integer n
    E.String s -> Just $ V.String s
    E.Primitive p -> Just $ getValue p
    where
        evalPatternMatching alts maybeArgVal = foldr f Nothing alts where
            f (altPatt, altExpr) acc = case match maybeArgVal altPatt of
                Just envExtension -> eval' (Map.union envExtension env) defs altExpr
                Nothing -> acc

match :: Eq c => Maybe (V.Value c) -> P.Pattern c -> Maybe (Map.Map E.VarName (Maybe (V.Value c)))
match maybeValue patt = case patt of
    P.Wildcard -> Just Map.empty
    P.Var var -> Just $ Map.singleton var maybeValue -- maybeValue is not evaluated (yet) in this case
    P.Constructor patternConstructorKey patterns -> case maybeValue of
        Just (V.Constructor valueConstructorKey values) | patternConstructorKey == valueConstructorKey ->
            mconcat <$> zipWithM match values patterns
        _ -> Nothing
    P.Integer n -> case maybeValue of
        Just (V.Integer m) | n == m -> Just Map.empty
        _ -> Nothing
    P.String s1 -> case maybeValue of
        Just (V.String s2) | s1 == s2 -> Just Map.empty
        _ -> Nothing
