module Diff where

import Control.Monad
import Data.Maybe
import qualified Data.List.NonEmpty as NonEmpty
import qualified Expr as E
import qualified Pattern as P

getDiffPathBetweenExprs :: (Eq c, Eq d) => E.Expr d c -> E.Expr d c -> Maybe E.Path
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
        E.Fn alts2 -> useChildDiffPaths $ join $ NonEmpty.toList $ NonEmpty.zipWith f alts1 alts2 where
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

getDiffPathBetweenPatterns :: Eq t => P.Pattern t -> P.Pattern t -> Maybe E.Path
getDiffPathBetweenPatterns p1 p2 = case p1 of
    P.Wildcard -> case p2 of
        P.Wildcard -> Nothing
        _ -> Just []
    P.Var v1 -> case p2 of
        P.Var v2 | v1 == v2 -> Nothing
        _ -> Just []
    P.Constructor k1 children1 -> case p2 of
        P.Constructor k2 children2 | k1 == k2 -> useChildDiffPaths $ zipWith getDiffPathBetweenPatterns children1 children2
        _ -> Just []
    P.Int n1 -> case p2 of
        P.Int n2 | n1 == n2 -> Nothing
        _ -> Just []

useChildDiffPaths :: [Maybe E.Path] -> Maybe E.Path
useChildDiffPaths childDiffPaths = case filter (isJust . snd) $ zip [0..] childDiffPaths of
    [] -> Nothing
    [(childIndex, childDiffPath)] -> (childIndex :) <$> childDiffPath
    _ -> Just []
