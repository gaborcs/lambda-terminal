module Eval where

import Control.Monad
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Expr as E
import qualified Pattern as P
import qualified Primitive
import qualified Value as V

eval :: Map.Map E.ExprName E.Expr -> E.Expr -> Maybe V.Value
eval = eval' Map.empty

eval' :: V.Env -> Map.Map E.ExprName E.Expr -> E.Expr -> Maybe V.Value
eval' env defs expr = case expr of
    E.Hole -> Nothing
    E.Ref ref -> Map.lookup ref defs >>= eval' env defs
    E.Var var -> join $ Map.lookup var env
    E.Fn alternatives -> Just $ V.Fn $ evalPatternMatching (NonEmpty.toList alternatives) where
        evalPatternMatching alternatives maybeArgVal = case alternatives of
            [] -> Nothing
            (pattern, expr):alts -> case match maybeArgVal pattern of
                Just envExtension -> eval' (Map.union envExtension env) defs expr
                Nothing -> evalPatternMatching alts maybeArgVal
    E.Call callee arg -> do
        calleeVal <- eval' env defs callee
        let maybeArgVal = eval' env defs arg -- evaluate lazily
        case calleeVal of
            V.Fn f -> f maybeArgVal
            V.Constructor name values -> Just $ V.Constructor name $ values ++ [maybeArgVal]
            _ -> Nothing
    E.Constructor name -> Just $ V.Constructor name []
    E.Int n -> Just $ V.Int n
    E.Primitive p -> Just $ Primitive.getValue p

match :: Maybe V.Value -> P.Pattern -> Maybe (Map.Map E.ExprName (Maybe V.Value))
match maybeValue pattern = case pattern of
    P.Wildcard -> Just $ Map.empty
    P.Var var -> Just $ Map.singleton var maybeValue -- maybeValue is not evaluated (yet) in this case
    P.Constructor name2 patterns -> case maybeValue of
        Just (V.Constructor name1 values) ->
            if name1 == name2 then mconcat <$> sequence (zipWith match values patterns) else Nothing
        _ -> Nothing
