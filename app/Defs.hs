module Defs where

import Data.List.NonEmpty
import Expr
import qualified Data.Map as Map
import qualified Pattern as P

defs :: Map.Map ExprName Expr
defs = Map.fromList
    [ ("if", fn "condition" $ fn "expr1" $ fn "expr2" $
        Call (Fn $ (P.Constructor "True" [], Var "expr1") :| [(P.Constructor "False" [], Var "expr2")]) (Var "condition"))
    , ("factorial", fn "n" $
        Call (Call (Call (Ref "if") (Call (Call Equals (Var "n")) (Int 0)))
             (Int 1))
             (Call (Call Times (Var "n")) (Call (Ref "factorial") (Call (Call Minus (Var "n")) (Int 1)))))
    , ("const", fn "x" . fn "y" $ Var "x")
    , ("increment", Call Plus $ Int 1)
    , ("main", Call (Call (Constructor "Just") (Call (Ref "factorial") (Call (Call (Call (Ref "const") (Ref "increment")) (Int 1)) (Int 2)))) (Int 3))
    ]
