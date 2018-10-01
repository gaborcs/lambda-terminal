module Defs where

import Data.List.NonEmpty
import Expr
import Primitive
import qualified Data.Map as Map
import qualified Pattern as P

defs :: Map.Map ExprName Expr
defs = Map.fromList
    [ ("case", fn "value" $ fn "matcher" $ Call (Var "matcher") (Var "value"))
    , ("if", fn "condition" $ fn "thenExpr" $ fn "elseExpr" $
        Call (Call (Ref "case") (Var "condition")) (Fn $ (P.Constructor "True" [], Var "thenExpr") :| [(P.Constructor "False" [], Var "elseExpr")]))
    , ("factorial", fn "n" $
        Call (Call (Call (Ref "if") (Call (Call (Primitive Equals) (Var "n")) (Int 0)))
             (Int 1))
             (Call (Call (Primitive Times) (Var "n")) (Call (Ref "factorial") (Call (Call (Primitive Minus) (Var "n")) (Int 1)))))
    , ("fibonacciSeq", Call (Call (Ref "fibonacciSeqFrom") (Int 1)) (Int 1))
    , ("fibonacciSeqFrom", fn "a" $ fn "b" $ Call (Call (Constructor "InfList.Cons") (Var "a")) (Call (Call (Ref "fibonacciSeqFrom") (Var "b")) (Call (Call (Primitive Plus) (Var "a")) (Var "b"))))
    , ("InfList.take", fn "n" $ Fn $ pure (P.Constructor "InfList.Cons" [P.Var "x", P.Var "xs"],
        Call (Call (Call (Ref "if") (Call (Call (Primitive Equals) (Var "n")) (Int 0))) (Constructor "[]"))
        (Call (Call (Constructor "Cons") (Var "x")) (Call (Call (Ref "InfList.take") (Call (Call (Primitive Minus) (Var "n")) (Int 1))) (Var "xs")))))
    , ("const", fn "x" . Fn $ pure (P.Wildcard, Var "x"))
    , ("increment", Call (Primitive Plus) $ Int 1) , ("main", Call (Call (Ref "const") (Call (Ref "factorial") (Call (Ref "increment") (Int 4)))) (Call (Call (Ref "InfList.take") (Int 7)) (Ref "fibonacciSeq")))
    ]
