module Expr where

import Primitive
import qualified Data.List.NonEmpty as NonEmpty
import qualified Pattern as P

type VarName = P.VarName
type PatternMatching defKey constructorKey = NonEmpty.NonEmpty (Alternative defKey constructorKey)
type Alternative defKey constructorKey = (P.Pattern constructorKey, Expr defKey constructorKey)

data Expr defKey constructorKey
    = Hole
    | Def defKey
    | Var VarName
    | Fn (PatternMatching defKey constructorKey)
    | Call (Expr defKey constructorKey) (Expr defKey constructorKey)
    | Constructor constructorKey
    | Int Int
    | Primitive Primitive
    deriving (Eq, Show)

-- shortcut for creating functions with a single alternative
fn :: VarName -> Expr d c -> Expr d c
fn var body = Fn $ pure (P.Var var, body)

type Path = [ChildIndex]
type ChildIndex = Int
