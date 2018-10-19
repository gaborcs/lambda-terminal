module PrettyPrintType (prettyPrintType) where

import Control.Monad
import qualified Type as T

prettyPrintType :: (t -> String) -> T.Type t -> String
prettyPrintType getName t = case t of
    T.Var varId -> typeVarNames !! varId
    T.Fn paramType resultType -> withParensIf isFn paramType ++ " -> " ++ prettyPrintType getName resultType
    T.Constructor typeDefKey types -> unwords $ getName typeDefKey : fmap (withParensIf isComplex) types
    T.Int -> "Int"
    where
        withParensIf pred t = if pred t then "(" ++ s ++ ")" else s where
            s = prettyPrintType getName t

typeVarNames :: [String]
typeVarNames = [1..] >>= flip replicateM ['a'..'z']

isFn :: T.Type t -> Bool
isFn t = case t of
    T.Fn _ _ -> True
    _ -> False

isComplex :: T.Type t -> Bool
isComplex t = case t of
    T.Var _ -> False
    T.Fn _ _ -> True
    T.Constructor _ types -> not $ null types
    T.Int -> False
