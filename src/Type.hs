{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Type where

import Control.DeepSeq
import Control.Lens
import Data.List
import GHC.Generics

type VarId = Int

data Type typeDefKey
    = Wildcard
    | Var VarId
    | Call (Type typeDefKey) (Type typeDefKey)
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
    , _typeConstructorParams :: [String]
    } deriving (Read, Show)

data DataConstructor typeDefKey = DataConstructor
    { _dataConstructorName :: String
    , _dataConstructorParamTypes :: [Type typeDefKey]
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

fn :: Type t -> Type t -> Type t
fn = Call . Call Fn

getDataConstructorType :: (t -> TypeDef t) -> DataConstructorKey t -> Maybe (Type t)
getDataConstructorType getTypeDef (DataConstructorKey typeDefKey constructorName) = foldr fn resultType <$> maybeParamTypes where
    TypeDef typeConstructor dataConstructors = getTypeDef typeDefKey
    resultType = foldl Call (Constructor typeDefKey) typeVars
    typeVars = Var <$> [0 .. length (view typeConstructorParams typeConstructor) - 1]
    maybeParamTypes = view dataConstructorParamTypes <$> maybeConstructorDef
    maybeConstructorDef = find ((== constructorName) . view dataConstructorName) dataConstructors
