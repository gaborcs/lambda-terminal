{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Value where

import GHC.Generics
import Control.DeepSeq
import qualified Data.Map as Map
import qualified Expr as E

-- the result of evaluation is a Maybe value because the evaluation may fail
-- passing around the Maybe values instead of the actual values allows us to keep them unevaluated until needed (lazy evaluation)

type Env = Map.Map E.VarName (Maybe Value)

data Value
    = Fn (Maybe Value -> Maybe Value)
    | Constructor E.ConstructorName [Maybe Value]
    | Int Int
    deriving (Generic, NFData)
