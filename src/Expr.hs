module Expr where

type ExprName = String
type VarName = String

data Expr
    = Ref ExprName
    | Var VarName
    | Fn VarName Expr
    | Call Expr Expr
    | Int Int
    deriving (Eq, Show)

type Path = [ChildIndex]
type ChildIndex = Int
