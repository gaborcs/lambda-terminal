module EvalSpec where

import Test.Hspec
import Eval
import qualified Data.Map as Map
import qualified Expr as E
import qualified Value as V

spec :: Spec
spec = it "evaluates expressions" $ do
    E.Int 1 `evaluatesTo` V.Int 1
    E.Int 2 `evaluatesTo` V.Int 2
    E.Call (E.Ref "id") (E.Int 1) `evaluatesTo` V.Int 1
    E.Call (E.Ref "constOne") (E.Int 2) `evaluatesTo` V.Int 1
    E.Call (E.Call (E.Ref "const") (E.Int 1)) (E.Int 2) `evaluatesTo` V.Int 1

defs :: Map.Map E.ExprName E.Expr
defs = Map.fromList
    [ ("id", E.Fn "x" $ E.Var "x")
    , ("constOne", E.Fn "x" $ E.Int 1)
    , ("const", E.Fn "x" . E.Fn "y" $ E.Var "x") ]

evaluatesTo :: E.Expr -> V.Value -> Expectation
evaluatesTo expr val = eval defs expr `shouldBe` Just val
