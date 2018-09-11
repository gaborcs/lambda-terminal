module Pattern where

type VarName = String
type ConstructorName = String

data Pattern
    = Var VarName
    | Constructor ConstructorName [Pattern]
    deriving (Eq, Show)
