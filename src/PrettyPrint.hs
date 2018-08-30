module PrettyPrint where

import Control.Monad
import qualified Type as T

prettyPrint :: T.Type -> String
prettyPrint t = case t of
    T.Var varId -> typeVarNames !! varId
    T.Fn paramType resultType -> case paramType of
        T.Fn _ _ -> "(" ++ prettyPrint paramType ++ ") -> " ++ prettyPrint resultType
        _ -> prettyPrint paramType ++ " -> " ++ prettyPrint resultType
    T.Int -> "Int"

typeVarNames :: [String]
typeVarNames = [1..] >>= flip replicateM ['a'..'z']