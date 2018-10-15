module Expr where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Pattern as P
import qualified Type as T
import qualified Value as V

type VarName = P.VarName
type ConstructorName = P.ConstructorName
type PatternMatching defId primitive = NonEmpty.NonEmpty (Alternative defId primitive)
type Alternative defId primitive = (P.Pattern, Expr defId primitive)

class Primitive p where
    getDisplayName :: p -> String
    getType :: p -> T.Type
    getValue :: p -> V.Value

data Expr defId primitive
    = Hole
    | Def defId
    | Var VarName
    | Fn (PatternMatching defId primitive)
    | Call (Expr defId primitive) (Expr defId primitive)
    | Constructor ConstructorName
    | Int Int
    | Primitive primitive
    deriving (Eq, Show)

-- shortcut for creating functions with a single alternative
fn :: VarName -> Expr d p -> Expr d p
fn var body = Fn $ pure (P.Var var, body)

type Path = [ChildIndex]
type ChildIndex = Int
