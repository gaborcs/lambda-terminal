module Pattern where

type VarName = String
type ConstructorName = String

data Pattern
    = Wildcard
    | Var VarName
    | Constructor ConstructorName [Pattern]
    | Int Int
    deriving (Eq, Show)
