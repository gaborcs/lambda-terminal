{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Type where

import Control.DeepSeq
import Control.Lens
import Data.List
import GHC.Generics

type VarName = String

data Type typeVarKey typeDefKey
    = Wildcard
    | Var typeVarKey
    | Call (Type typeVarKey typeDefKey) (Type typeVarKey typeDefKey)
    | Constructor typeDefKey
    | Fn
    | Int
    deriving (Eq, Read, Show, Functor)

data TypeDef typeDefKey = TypeDef
    { _typeConstructor :: TypeConstructor
    , _dataConstructors :: [DataConstructor typeDefKey]
    } deriving (Read, Show)

data TypeConstructor = TypeConstructor
    { _typeConstructorName :: Maybe String
    , _typeConstructorParams :: [VarName]
    } deriving (Eq, Read, Show)

data DataConstructor typeDefKey = DataConstructor
    { _dataConstructorName :: String
    , _dataConstructorParamTypes :: [Type VarName typeDefKey]
    } deriving (Read, Show, Functor)

data DataConstructorKey typeDefKey = DataConstructorKey
    { _typeDefKey :: typeDefKey
    , _constructorName :: String
    } deriving (Eq, Read, Show, Generic, NFData)

makePrisms ''Type
makeLenses ''TypeDef
makeLenses ''TypeConstructor
makeLenses ''DataConstructor
makeLenses ''DataConstructorKey

fn :: Type v d -> Type v d -> Type v d
fn = Call . Call Fn

getDataConstructorType :: (d -> TypeDef d) -> DataConstructorKey d -> Maybe (Type VarName d)
getDataConstructorType getTypeDef (DataConstructorKey typeDefKey constructorName) = foldr fn resultType <$> maybeParamTypes where
    TypeDef typeConstructor dataConstructors = getTypeDef typeDefKey
    resultType = foldl Call (Constructor typeDefKey) $ Var <$> view typeConstructorParams typeConstructor
    maybeParamTypes = view dataConstructorParamTypes <$> maybeConstructorDef
    maybeConstructorDef = find ((== constructorName) . view dataConstructorName) dataConstructors
