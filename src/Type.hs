module Type where

type VarId = Int

data Type
    = Var VarId
    | Fn Type Type
    | Int
    deriving (Eq, Show)
