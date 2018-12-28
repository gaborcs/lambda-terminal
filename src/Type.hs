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
    | Integer
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

getTypeVars :: Eq v => Type v d -> [v]
getTypeVars t = case t of
    Wildcard -> []
    Var var -> [var]
    Call a b -> getTypeVars a `union` getTypeVars b
    Constructor _ -> []
    Fn -> []
    Integer -> []

mapTypeVars :: (v1 -> Type v2 d) -> Type v1 d -> Type v2 d
mapTypeVars f t = case t of
    Wildcard -> Wildcard
    Var var -> f var
    Call a b -> Call (mapTypeVars f a) (mapTypeVars f b)
    Constructor name -> Constructor name
    Fn -> Fn
    Integer -> Integer

getTypeVarsInTypeDef :: TypeDef d -> [VarName]
getTypeVarsInTypeDef def = nub $
    (def ^. typeConstructor . typeConstructorParams)
    ++ (def ^. dataConstructors >>= view dataConstructorParamTypes >>= getTypeVars)

getDataConstructorType :: (d -> TypeDef d) -> DataConstructorKey d -> Maybe (Type VarName d)
getDataConstructorType getTypeDef (DataConstructorKey typeDefKey constructorName) = foldr fn resultType <$> maybeParamTypes where
    TypeDef typeConstructor dataConstructors = getTypeDef typeDefKey
    resultType = foldl Call (Constructor typeDefKey) $ Var <$> view typeConstructorParams typeConstructor
    maybeParamTypes = view dataConstructorParamTypes <$> maybeConstructorDef
    maybeConstructorDef = find ((== constructorName) . view dataConstructorName) dataConstructors

renameTypeVar :: VarName -> VarName -> TypeDef d -> TypeDef d
renameTypeVar oldName newName =
    over (typeConstructor . typeConstructorParams) (map replaceIfMatch) .
    over dataConstructors (map $ over dataConstructorParamTypes $ map renameInType)
    where
        replaceIfMatch name = if name == oldName then newName else name
        renameInType = mapTypeVars $ Var . replaceIfMatch
