module PrettyPrintType where

import Control.Lens.Extras
import Control.Monad
import Util
import qualified Type as T

prettyPrintType :: (t -> String) -> [String] -> T.Type t -> String
prettyPrintType getTypeName typeVarNames t = case t of
    T.Var varId -> typeVarNames !! varId
    T.Fn paramType resultType -> inParensIf (is T._Fn paramType) (print paramType) ++ " -> " ++ print resultType
    T.Constructor typeDefKey children -> unwords $ getTypeName typeDefKey : fmap printChild children where
        printChild child = inParensIf (isMultiWord child) (print child)
    T.Int -> "Int"
    where print = prettyPrintType getTypeName typeVarNames

defaultTypeVarNames :: [String]
defaultTypeVarNames = [1..] >>= flip replicateM ['a'..'z']

isMultiWord :: T.Type t -> Bool
isMultiWord t = case t of
    T.Var _ -> False
    T.Fn _ _ -> True
    T.Constructor _ types -> not $ null types
    T.Int -> False
