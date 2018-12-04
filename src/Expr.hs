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
    | Integer Integer
    | Primitive Primitive
    deriving (Eq, Read, Show)

-- shortcut for creating functions with a single alternative
fn :: VarName -> Expr d c -> Expr d c
fn var body = Fn $ pure (P.Var var, body)

renameVar :: VarName -> VarName -> Expr d c -> Expr d c
renameVar oldName newName expr = case expr of
    Var name | name == oldName -> Var newName
    Fn alts -> Fn $ renameAlt <$> alts where
        renameAlt (p, e) = (P.renameVar oldName newName p, renameVar oldName newName e)
    Call callee arg -> Call (renameVar oldName newName callee) (renameVar oldName newName arg)
    _ -> expr
