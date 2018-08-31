module Value where

import qualified Data.Map as Map
import qualified Expr as E

type Env = Map.Map E.VarName Value

data Value
    = Fn (Value -> Maybe Value)
    | Int Int
