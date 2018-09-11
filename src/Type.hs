module Type where

import qualified Expr as E

type VarId = Int

data Type
    = Var VarId
    | Fn Type Type
    | Constructor E.ConstructorName [Type]
    | Int
    deriving (Eq, Show)
