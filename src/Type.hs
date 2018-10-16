module Type where

type VarId = Int

data Type typeDefKey
    = Var VarId
    | Fn (Type typeDefKey) (Type typeDefKey)
    | Constructor typeDefKey [Type typeDefKey]
    | Int
    deriving (Eq, Show)
