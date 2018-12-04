module PrettyPrintType where

import Control.Lens.Extras
import Control.Monad
import qualified Type as T

prettyPrintType :: (d -> String) -> T.Type String d -> String
prettyPrintType getTypeName t = case t of
    T.Wildcard -> "_"
    T.Var name -> name
    T.Call callee arg -> print callee ++ " " ++ if isMultiWord arg then "(" ++ print arg ++ ")" else print arg
    T.Constructor typeDefKey -> getTypeName typeDefKey
    T.Fn -> "λ"
    T.Integer -> "Integer"
    where print = prettyPrintType getTypeName

defaultTypeVarNames :: [String]
defaultTypeVarNames = [1..] >>= flip replicateM ['a'..'z']

isMultiWord :: T.Type v d -> Bool
isMultiWord = is T._Call
