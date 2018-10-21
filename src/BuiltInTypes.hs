{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module BuiltInTypes where

import Control.DeepSeq
import GHC.Generics
import Type

data TypeDefKey
    = Bool
    | Maybe
    | List
    | InfList
    deriving (Eq, Read, Show, Enum, Bounded, Generic, NFData)

getTypeDef :: TypeDefKey -> TypeDef TypeDefKey
getTypeDef key = case key of
    Bool -> TypeDef [] [DataConstructor "False" [], DataConstructor "True" []]
    Maybe -> TypeDef ["a"] [DataConstructor "Nothing" [], DataConstructor "Just" [Var 0]]
    List -> TypeDef ["a"] [DataConstructor "[]" [], DataConstructor "Cons" [Var 0, Constructor List [Var 0]]]
    InfList -> TypeDef ["a"] [DataConstructor "Cons" [Var 0, Constructor InfList [Var 0]]]

typeDefKeys :: [TypeDefKey]
typeDefKeys = [minBound..]

type DataConstructorKey = Type.DataConstructorKey TypeDefKey