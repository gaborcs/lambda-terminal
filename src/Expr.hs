module Expr where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Pattern as P

type ExprName = String
type VarName = P.VarName
type ConstructorName = P.ConstructorName
type PatternMatching = NonEmpty.NonEmpty Alternative
type Alternative = (P.Pattern, Expr)

data Expr
    = Ref ExprName
    | Var VarName
    | Fn PatternMatching
    | Call Expr Expr
    | Constructor ConstructorName
    | Int Int
    | Plus
    | Times
    deriving (Eq, Show)

-- shortcut for creating functions with a single alternative
fn :: VarName -> Expr -> Expr
fn var body = Fn $ pure (P.Var var, body)

type Path = [ChildIndex]
type ChildIndex = Int
