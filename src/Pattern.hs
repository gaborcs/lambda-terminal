module Pattern where

type VarName = String
type ConstructorName = String

data Pattern constructorKey
    = Wildcard
    | Var VarName
    | Constructor constructorKey [Pattern constructorKey]
    | Int Int
    deriving (Eq, Show)
