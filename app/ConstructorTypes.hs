module ConstructorTypes where

import Data.Map
import Type
import qualified Expr as E

constructorTypes :: Map E.ConstructorName Type
constructorTypes = fromList
    [ ("False", Constructor "Bool" [])
    , ("True", Constructor "Bool" [])
    , ("Nothing", Constructor "Maybe" [Var 0])
    , ("Just", Fn (Var 0) (Constructor "Maybe" [Var 0]))
    , ("[]", Constructor "List" [Var 0])
    , ("Cons", Fn (Var 0) (Fn (Constructor "List" [Var 0]) (Constructor "List" [Var 0])))
    , ("InfList.Cons", Fn (Var 0) (Fn (Constructor "InfList" [Var 0]) (Constructor "InfList" [Var 0])))
    ]
