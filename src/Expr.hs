module Expr where

import Primitive
import qualified Data.List.NonEmpty as NonEmpty
import qualified Pattern as P

type VarName = P.VarName
type ConstructorName = P.ConstructorName
type PatternMatching defId = NonEmpty.NonEmpty (Alternative defId)
type Alternative defId = (P.Pattern, Expr defId)

data Expr defId
    = Hole
    | Def defId
    | Var VarName
    | Fn (PatternMatching defId)
    | Call (Expr defId) (Expr defId)
    | Constructor ConstructorName
    | Int Int
    | Primitive Primitive
    deriving (Eq, Show)

-- shortcut for creating functions with a single alternative
fn :: VarName -> Expr d -> Expr d
fn var body = Fn $ pure (P.Var var, body)

type Path = [ChildIndex]
type ChildIndex = Int
