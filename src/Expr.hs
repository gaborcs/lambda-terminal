module Expr where

import Primitive
import qualified Data.List.NonEmpty as NonEmpty
import qualified Pattern as P

type VarName = P.VarName
type ConstructorName = P.ConstructorName
type PatternMatching d = NonEmpty.NonEmpty (Alternative d)
type Alternative d = (P.Pattern, Expr d)

data Expr d -- d is the type of definition identifiers
    = Hole
    | Def d
    | Var VarName
    | Fn (PatternMatching d)
    | Call (Expr d) (Expr d)
    | Constructor ConstructorName
    | Int Int
    | Primitive Primitive
    deriving (Eq, Show)

-- shortcut for creating functions with a single alternative
fn :: VarName -> Expr d -> Expr d
fn var body = Fn $ pure (P.Var var, body)

type Path = [ChildIndex]
type ChildIndex = Int
