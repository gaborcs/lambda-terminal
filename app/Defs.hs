module Defs where

import Data.List.NonEmpty
import Expr
import BuiltInPrimitives
import qualified BuiltInTypes as BT
import qualified Data.Map as Map
import qualified Pattern as P

data DefKey
    = IfD
    | FactorialD
    | FibonacciSeqD
    | FibonacciSeqFromD
    | InfListTakeD
    | ConstD
    | IncrementD
    deriving (Eq, Ord)

defs :: Map.Map DefKey (String, Expr DefKey BT.ConstructorKey Primitive)
defs = Map.fromList
    [ (IfD, ("if", fn "condition" $ fn "thenExpr" $ fn "elseExpr" $ Call
        (Fn $
            (P.Constructor (BT.ConstructorKey BT.Bool "True") [], Var "thenExpr") :|
            [(P.Constructor (BT.ConstructorKey BT.Bool "False") [], Var "elseExpr")])
        (Var "condition")))
    , (FactorialD, ("factorial", fn "n" $
        Call (Call (Call (Def IfD) (Call (Call (Primitive Equals) (Var "n")) (Int 0)))
             (Int 1))
             (Call (Call (Primitive Times) (Var "n")) (Call (Def FactorialD) (Call (Call (Primitive Minus) (Var "n")) (Int 1))))))
    , (FibonacciSeqD, ("fibonacciSeq", Call (Call (Def FibonacciSeqFromD) (Int 1)) (Int 1)))
    , (FibonacciSeqFromD, ("fibonacciSeqFrom", fn "a" $ fn "b" $
        Call (Call (Constructor (BT.ConstructorKey BT.InfList "Cons")) (Var "a"))
            (Call (Call (Def FibonacciSeqFromD) (Var "b")) (Call (Call (Primitive Plus) (Var "a")) (Var "b")))))
    , (InfListTakeD, ("InfList.take", fn "n" $ Fn $ pure (P.Constructor (BT.ConstructorKey BT.InfList "Cons") [P.Var "x", P.Var "xs"],
        Call (Call (Call (Def IfD) (Call (Call (Primitive Equals) (Var "n")) (Int 0))) (Constructor (BT.ConstructorKey BT.List "[]")))
        (Call (Call (Constructor (BT.ConstructorKey BT.List "Cons")) (Var "x"))
        (Call (Call (Def InfListTakeD) (Call (Call (Primitive Minus) (Var "n")) (Int 1))) (Var "xs"))))))
    , (ConstD, ("const", fn "x" . Fn $ pure (P.Wildcard, Var "x")))
    , (IncrementD, ("increment", Call (Primitive Plus) $ Int 1))
    ]
