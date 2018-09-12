module Defs where

import Expr
import Data.Map

defs :: Map ExprName Expr
defs = fromList
    [ ("const", fn "x" . fn "y" $ Var "x")
    , ("increment", Call Plus $ Int 1)
    , ("square", fn "n" $ Call (Call Times (Var "n")) (Var "n"))
    , ("main", Call (Call (Constructor "Just") (Call (Ref "square") (Call (Call (Call (Ref "const") (Ref "increment")) (Int 1)) (Int 2)))) (Int 3))
    ]
