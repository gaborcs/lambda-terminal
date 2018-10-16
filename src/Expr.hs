{-# LANGUAGE MultiParamTypeClasses #-}

module Expr where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Pattern as P
import qualified Type as T
import qualified Value as V

type VarName = P.VarName
type PatternMatching defKey constructorKey primitive = NonEmpty.NonEmpty (Alternative defKey constructorKey primitive)
type Alternative defKey constructorKey primitive = (P.Pattern constructorKey, Expr defKey constructorKey primitive)

class PrimitiveName p where
    getDisplayName :: p -> String
class PrimitiveType p t where
    getType :: p -> T.Type t
class PrimitiveValue p c where
    getValue :: p -> V.Value c

data Expr defKey constructorKey primitive
    = Hole
    | Def defKey
    | Var VarName
    | Fn (PatternMatching defKey constructorKey primitive)
    | Call (Expr defKey constructorKey primitive) (Expr defKey constructorKey primitive)
    | Constructor constructorKey
    | Int Int
    | Primitive primitive
    deriving (Eq, Show)

-- shortcut for creating functions with a single alternative
fn :: VarName -> Expr d c p -> Expr d c p
fn var body = Fn $ pure (P.Var var, body)

type Path = [ChildIndex]
type ChildIndex = Int
