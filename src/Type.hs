{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Type where

import Control.DeepSeq
import Control.Lens
import Data.List
import GHC.Generics

type VarId = Int

data Type typeDefKey
    = Var VarId
    | Fn (Type typeDefKey) (Type typeDefKey)
    | Constructor typeDefKey [Type typeDefKey]
    | Int
    deriving (Eq, Show, Functor)

data TypeDef typeDefKey = TypeDef
    { _varNames :: [String]
    , _dataConstructors :: [DataConstructor typeDefKey]
    }

data DataConstructor typeDefKey = DataConstructor
    { _dataConstructorName :: String
    , _dataConstructorParamTypes :: [Type typeDefKey]
    } deriving Functor

data DataConstructorKey typeDefKey = DataConstructorKey
    { _typeDefKey :: typeDefKey
    , _constructorName :: String
    } deriving (Eq, Show, Generic, NFData)

makeLenses ''TypeDef
makeLenses ''DataConstructor
makeLenses ''DataConstructorKey

getConstructorType :: (t -> TypeDef t) -> DataConstructorKey t -> Maybe (Type t)
getConstructorType getTypeDef (DataConstructorKey typeDefKey constructorName) = foldr Fn resultType <$> maybeParamTypes where
    TypeDef typeVarNames constructors = getTypeDef typeDefKey
    resultType = Constructor typeDefKey typeVars
    typeVars = Var <$> [0 .. length typeVarNames - 1]
    maybeParamTypes = view dataConstructorParamTypes <$> maybeConstructorDef
    maybeConstructorDef = find ((== constructorName) . view dataConstructorName) constructors
