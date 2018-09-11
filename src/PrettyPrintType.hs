module PrettyPrintType where

import Control.Monad
import qualified Type as T

type IsComplex = Bool

prettyPrintType :: T.Type -> String
prettyPrintType t = case t of
    T.Var varId -> typeVarNames !! varId
    T.Fn paramType resultType -> withParensIf isFn paramType ++ " -> " ++ prettyPrintType resultType
    T.Constructor name types -> unwords $ name : fmap (withParensIf isComplex) types
    T.Int -> "Int"

typeVarNames :: [String]
typeVarNames = [1..] >>= flip replicateM ['a'..'z']

withParensIf :: (T.Type -> Bool) -> T.Type -> String
withParensIf pred t = if pred t then "(" ++ s ++ ")" else s where
    s = prettyPrintType t

isFn :: T.Type -> Bool
isFn t = case t of
    T.Fn _ _ -> True
    _ -> False

isComplex :: T.Type -> Bool
isComplex t = case t of
    T.Var _ -> False
    T.Fn _ _ -> True
    T.Constructor _ types -> not $ null types
    T.Int -> False
