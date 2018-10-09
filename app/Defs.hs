module Defs where

import Data.List.NonEmpty
import Expr
import Primitive
import qualified Data.Map as Map
import qualified Pattern as P

defs :: Map.Map Int (String, Expr Int)
defs = Map.fromList
    [ (0, ("if", fn "condition" $ fn "thenExpr" $ fn "elseExpr" $
        Call (Fn $ (P.Constructor "True" [], Var "thenExpr") :| [(P.Constructor "False" [], Var "elseExpr")]) (Var "condition")))
    , (1, ("factorial", fn "n" $
        Call (Call (Call (Def 0) (Call (Call (Primitive Equals) (Var "n")) (Int 0)))
             (Int 1))
             (Call (Call (Primitive Times) (Var "n")) (Call (Def 1) (Call (Call (Primitive Minus) (Var "n")) (Int 1))))))
    , (2, ("fibonacciSeq", Call (Call (Def 3) (Int 1)) (Int 1)))
    , (3, ("fibonacciSeqFrom", fn "a" $ fn "b" $ Call (Call (Constructor "InfList.Cons") (Var "a")) (Call (Call (Def 3) (Var "b")) (Call (Call (Primitive Plus) (Var "a")) (Var "b")))))
    , (4, ("InfList.take", fn "n" $ Fn $ pure (P.Constructor "InfList.Cons" [P.Var "x", P.Var "xs"],
        Call (Call (Call (Def 0) (Call (Call (Primitive Equals) (Var "n")) (Int 0))) (Constructor "[]"))
        (Call (Call (Constructor "Cons") (Var "x")) (Call (Call (Def 4) (Call (Call (Primitive Minus) (Var "n")) (Int 1))) (Var "xs"))))))
    , (5, ("const", fn "x" . Fn $ pure (P.Wildcard, Var "x")))
    , (6, ("increment", Call (Primitive Plus) $ Int 1))
    ]
