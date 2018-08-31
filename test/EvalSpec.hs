module EvalSpec where

import Test.Hspec
import Eval
import qualified Data.Map as Map
import qualified Expr as E
import qualified Value as V

spec :: Spec
spec = it "evaluates expressions" $ do
    E.Int 1 `evaluatesToInt` 1
    E.Int 2 `evaluatesToInt` 2
    E.Call (E.Ref "id") (E.Int 1) `evaluatesToInt` 1
    E.Call (E.Ref "constOne") (E.Int 2) `evaluatesToInt` 1
    E.Call (E.Call (E.Ref "const") (E.Int 1)) (E.Int 2) `evaluatesToInt` 1
    E.Call (E.Ref "inc") (E.Int 2) `evaluatesToInt` 3

defs :: Map.Map E.ExprName E.Expr
defs = Map.fromList
    [ ("id", E.Fn "x" $ E.Var "x")
    , ("constOne", E.Fn "x" $ E.Int 1)
    , ("const", E.Fn "x" . E.Fn "y" $ E.Var "x")
    , ("inc", E.Call E.Plus (E.Int 1)) ]

evaluatesToInt :: E.Expr -> Int -> Expectation
evaluatesToInt expr val = case eval defs expr of
    Just (V.Int n) -> n `shouldBe` val
    _ -> expectationFailure $ show expr ++ " should evaluate to Int " ++ show val
