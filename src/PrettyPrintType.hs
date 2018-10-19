module PrettyPrintType (prettyPrintType) where

import Control.Lens.Extras
import Control.Monad
import qualified Type as T

prettyPrintType :: (t -> String) -> T.Type t -> String
prettyPrintType getName t = case t of
    T.Var varId -> typeVarNames !! varId
    T.Fn paramType resultType -> withParensIf (is T._Fn) paramType ++ " -> " ++ prettyPrintType getName resultType
    T.Constructor typeDefKey types -> unwords $ getName typeDefKey : fmap (withParensIf isMultiWord) types
    T.Int -> "Int"
    where
        withParensIf pred t = if pred t then "(" ++ s ++ ")" else s where
            s = prettyPrintType getName t

typeVarNames :: [String]
typeVarNames = [1..] >>= flip replicateM ['a'..'z']

isMultiWord :: T.Type t -> Bool
isMultiWord t = case t of
    T.Var _ -> False
    T.Fn _ _ -> True
    T.Constructor _ types -> not $ null types
    T.Int -> False
