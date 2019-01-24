{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Value where

import GHC.Generics
import Control.DeepSeq
import qualified Data.Map as Map

-- the result of evaluation is a Maybe value because the evaluation may fail
-- passing around the Maybe values instead of the actual values allows us to keep them unevaluated until needed (lazy evaluation)

type Env constructorKey = Map.Map String (Maybe (Value constructorKey))

data Value constructorKey
    = Fn (Maybe (Value constructorKey) -> Maybe (Value constructorKey))
    | Constructor constructorKey [Maybe (Value constructorKey)]
    | Integer Integer
    | String String
    deriving (Generic, NFData)
