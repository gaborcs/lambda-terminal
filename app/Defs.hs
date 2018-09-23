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
    , ("fibonacciSeq", Call (Call (Ref "fibonacciSeqFrom") (Int 1)) (Int 1))
    , ("fibonacciSeqFrom", fn "a" $ fn "b" $ Call (Call (Constructor "InfList.Cons") (Var "a")) (Call (Call (Ref "fibonacciSeqFrom") (Var "b")) (Call (Call Plus (Var "a")) (Var "b"))))
    , ("InfList.take", fn "n" $ Fn $ pure (P.Constructor "InfList.Cons" [P.Var "x", P.Var "xs"],
        Call (Call (Call (Ref "if") (Call (Call Equals (Var "n")) (Int 0))) (Constructor "[]"))
        (Call (Call (Constructor "Cons") (Var "x")) (Call (Call (Ref "InfList.take") (Call (Call Minus (Var "n")) (Int 1))) (Var "xs")))))
    , ("const", fn "x" . fn "y" $ Var "x")
    , ("increment", Call Plus $ Int 1)
    , ("main", Call (Call (Ref "const") (Call (Ref "factorial") (Call (Ref "increment") (Int 4)))) (Call (Call (Ref "InfList.take") (Int 7)) (Ref "fibonacciSeq")))
    ]
