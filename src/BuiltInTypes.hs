{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module BuiltInTypes where

import Control.Lens hiding (List)
import Control.DeepSeq
import GHC.Generics
import Data.Maybe
import Type

data TypeDefKey
    = Bool
    | Maybe
    | List
    | InfList
    deriving (Eq, Show, Enum, Bounded, Generic, NFData)

getTypeDef :: TypeDefKey -> TypeDef
getTypeDef key = case key of
    Bool -> TypeDef [] [("False", []), ("True", [])]
    Maybe -> TypeDef ["a"] [("Nothing", []), ("Just", [Var 0])]
    List -> TypeDef ["a"] [("[]", []), ("Cons", [Var 0, Constructor List [Var 0]])]
    InfList -> TypeDef ["a"] [("Cons", [Var 0, Constructor InfList [Var 0]])]

data TypeDef = TypeDef
    { _varNames :: [String]
    , _constructors :: [ConstructorDef]
    }
type ConstructorDef = (ConstructorName, [ParamType])
type ConstructorName = String
type ParamType = Type TypeDefKey
data ConstructorKey = ConstructorKey
    { _typeDefKey :: TypeDefKey
    , _constructorName :: ConstructorName
    } deriving (Eq, Show, Generic, NFData)

makeLenses ''TypeDef
makeLenses ''ConstructorKey

typeDefKeys :: [TypeDefKey]
typeDefKeys = [minBound..]

getConstructorType :: ConstructorKey -> Maybe (Type TypeDefKey)
getConstructorType (ConstructorKey typeDefKey constructorName) = foldr Fn resultType <$> maybeParamTypes where
    TypeDef typeVarNames constructors = getTypeDef typeDefKey
    resultType = Constructor typeDefKey typeVars
    typeVars = Var <$> [0 .. length typeVarNames - 1]
    maybeParamTypes = lookup constructorName constructors
