module ConstructorTypes where

import Data.Map
import Type
import qualified Expr as E

constructorTypes :: Map E.ConstructorName Type
constructorTypes = fromList
    [ ("Nothing", Constructor "Maybe" [Var 0])
    , ("Just", Fn (Var 0) (Constructor "Maybe" [Var 0]))
    ]
