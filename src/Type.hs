module Type where

type VarId = Int

data Type
    = Var VarId
    | Fn Type Type
    | Constructor String [Type]
    | Int
    deriving (Eq, Show)
