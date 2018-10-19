module Eval where

import Primitive
import Control.Monad
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Expr as E
import qualified Pattern as P
import qualified Value as V

eval :: (Eq c, Ord d, PrimitiveValue c) => Map.Map d (E.Expr d c) -> E.Expr d c -> Maybe (V.Value c)
eval = eval' Map.empty

eval' :: (Eq c, Ord d, PrimitiveValue c) => V.Env c -> Map.Map d (E.Expr d c) -> E.Expr d c -> Maybe (V.Value c)
eval' env defs expr = case expr of
    E.Hole -> Nothing
    E.Def defId -> Map.lookup defId defs >>= eval' env defs
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
    E.Int n -> Just $ V.Int n
    E.Primitive p -> Just $ getValue p
    where
        evalPatternMatching alts maybeArgVal = case alts of
            [] -> Nothing
            (firstAltPatt, firstAltExpr):restOfAlts -> case match maybeArgVal firstAltPatt of
                Just envExtension -> eval' (Map.union envExtension env) defs firstAltExpr
                Nothing -> evalPatternMatching restOfAlts maybeArgVal

match :: Eq c => Maybe (V.Value c) -> P.Pattern c -> Maybe (Map.Map E.VarName (Maybe (V.Value c)))
match maybeValue patt = case patt of
    P.Wildcard -> Just Map.empty
    P.Var var -> Just $ Map.singleton var maybeValue -- maybeValue is not evaluated (yet) in this case
    P.Constructor patternConstructorKey patterns -> case maybeValue of
        Just (V.Constructor valueConstructorKey values) ->
            if patternConstructorKey == valueConstructorKey then mconcat <$> zipWithM match values patterns else Nothing
        _ -> Nothing
    P.Int n -> case maybeValue of
        Just (V.Int m) | n == m -> Just Map.empty
        _ -> Nothing
