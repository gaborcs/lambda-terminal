module Diff where

import Control.Monad
import Data.Maybe
import qualified Data.List.NonEmpty as NonEmpty
import qualified Expr as E
import qualified Pattern as P
import qualified Type as T

type Path = [ChildIndex]
type ChildIndex = Int

getDiffPathBetweenDataConstructors :: Eq d => T.DataConstructor d -> T.DataConstructor d -> Maybe Path
getDiffPathBetweenDataConstructors (T.DataConstructor name1 params1) (T.DataConstructor name2 params2) =
    if name1 == name2 && length params1 == length params2
        then useChildDiffPaths $ zipWith getDiffPathBetweenTypes params1 params2
        else Just []

getDiffPathBetweenTypes :: (Eq v, Eq d) => T.Type v d -> T.Type v d -> Maybe Path
getDiffPathBetweenTypes t1 t2 = case t1 of
    T.Wildcard -> case t2 of
        T.Wildcard -> Nothing
        _ -> Just []
    T.Var key1 -> case t2 of
        T.Var key2 | key1 == key2 -> Nothing
        _ -> Just []
    T.Call callee1 arg1 -> case t2 of
        T.Call callee2 arg2 -> useChildDiffPaths [getDiffPathBetweenTypes callee1 callee2, getDiffPathBetweenTypes arg1 arg2]
        _ -> Just []
    T.Constructor key1 -> case t2 of
        T.Constructor key2 | key1 == key2 -> Nothing
        _ -> Just []
    T.Fn -> case t2 of
        T.Fn -> Nothing
        _ -> Just []
    T.Int -> case t2 of
        T.Int -> Nothing
        _ -> Just []

getDiffPathBetweenExprs :: (Eq c, Eq d) => E.Expr d c -> E.Expr d c -> Maybe Path
getDiffPathBetweenExprs e1 e2 = case e1 of
    E.Hole -> case e2 of
        E.Hole -> Nothing
        _ -> Just []
    E.Def key1 -> case e2 of
        E.Def key2 | key1 == key2 -> Nothing
        _ -> Just []
    E.Var v1 -> case e2 of
        E.Var v2 | v1 == v2 -> Nothing
        _ -> Just []
    E.Fn alts1 -> case e2 of
        E.Fn alts2 | length alts1 == length alts2 -> useChildDiffPaths childDiffPaths where
            childDiffPaths = join $ NonEmpty.toList $ NonEmpty.zipWith f alts1 alts2
            f (p1, b1) (p2, b2) = [getDiffPathBetweenPatterns p1 p2, getDiffPathBetweenExprs b1 b2]
        _ -> Just []
    E.Call callee1 arg1 -> case e2 of
        E.Call callee2 arg2 -> useChildDiffPaths [getDiffPathBetweenExprs callee1 callee2, getDiffPathBetweenExprs arg1 arg2]
        _ -> Just []
    E.Constructor k1 -> case e2 of
        E.Constructor k2 | k1 == k2 -> Nothing
        _ -> Just []
    E.Int n1 -> case e2 of
        E.Int n2 | n1 == n2 -> Nothing
        _ -> Just []
    E.Primitive p1 -> case e2 of
        E.Primitive p2 | p1 == p2 -> Nothing
        _ -> Just []

getDiffPathBetweenPatterns :: Eq t => P.Pattern t -> P.Pattern t -> Maybe Path
getDiffPathBetweenPatterns p1 p2 = case p1 of
    P.Wildcard -> case p2 of
        P.Wildcard -> Nothing
        _ -> Just []
    P.Var v1 -> case p2 of
        P.Var v2 | v1 == v2 -> Nothing
        _ -> Just []
    P.Constructor k1 children1 -> case p2 of
        P.Constructor k2 children2 | k1 == k2 && length children1 == length children2 ->
            useChildDiffPaths $ zipWith getDiffPathBetweenPatterns children1 children2
        _ -> Just []
    P.Int n1 -> case p2 of
        P.Int n2 | n1 == n2 -> Nothing
        _ -> Just []

useChildDiffPaths :: [Maybe Path] -> Maybe Path
useChildDiffPaths childDiffPaths = case filter (isJust . snd) $ zip [0..] childDiffPaths of
    [] -> Nothing
    [(childIndex, childDiffPath)] -> (childIndex :) <$> childDiffPath
    _ -> Just []
