module PrettyPrintType where

import Control.Lens.Extras
import Control.Monad
import Util
import qualified Type as T

prettyPrintType :: (t -> String) -> [String] -> T.Type t -> String
prettyPrintType getTypeName typeVarNames t = case t of
    T.Wildcard -> "_"
    T.Var varId -> typeVarNames !! varId
    T.Fn paramType resultType -> inParensIf (is T._Fn paramType) (print paramType) ++ " -> " ++ print resultType
    T.Call callee arg -> print callee ++ " " ++ inParensIf (isMultiWord arg) (print arg)
    T.Constructor typeDefKey -> getTypeName typeDefKey
    T.Int -> "Int"
    where print = prettyPrintType getTypeName typeVarNames

defaultTypeVarNames :: [String]
defaultTypeVarNames = [1..] >>= flip replicateM ['a'..'z']

isMultiWord :: T.Type t -> Bool
isMultiWord t = case t of
    T.Wildcard -> False
    T.Var _ -> False
    T.Fn _ _ -> True
    T.Call _ _ -> True
    T.Constructor _ -> False
    T.Int -> False
