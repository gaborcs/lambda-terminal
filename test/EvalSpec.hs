module EvalSpec where

import Test.Hspec
import Eval
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Expr as E
import qualified Pattern as P
import qualified Value as V

spec :: Spec
spec = it "evaluates expressions" $ do
    E.Int 1 `evaluatesToInt` 1
    E.Int 2 `evaluatesToInt` 2
    E.Call (E.Ref "id") (E.Int 1) `evaluatesToInt` 1
    E.Call (E.Ref "constOne") (E.Int 2) `evaluatesToInt` 1
    E.Call (E.Call (E.Ref "const") (E.Int 1)) (E.Int 2) `evaluatesToInt` 1
    E.Call (E.Ref "inc") (E.Int 2) `evaluatesToInt` 3
    E.Call (E.Ref "boolToInt") (E.Constructor "False") `evaluatesToInt` 0
    E.Call (E.Ref "boolToInt") (E.Constructor "True") `evaluatesToInt` 1
    E.Call (E.Call (E.Ref "fromMaybe") (E.Int 0)) (E.Constructor "Nothing") `evaluatesToInt` 0
    E.Call (E.Call (E.Ref "fromMaybe") (E.Int 0)) (E.Call (E.Constructor "Just") (E.Int 1)) `evaluatesToInt` 1

defs :: Map.Map E.ExprName E.Expr
defs = Map.fromList
    [ ("id", E.fn "x" $ E.Var "x")
    , ("constOne", E.fn "x" $ E.Int 1)
    , ("const", E.fn "x" . E.fn "y" $ E.Var "x")
    , ("inc", E.Call E.Plus (E.Int 1))
    , ("boolToInt", E.Fn ((P.Constructor "False" [], E.Int 0) NonEmpty.:| [(P.Constructor "True" [], E.Int 1)]))
    , ("fromMaybe", E.fn "default" (E.Fn ((P.Constructor "Just" [P.Var "x"], E.Var "x") NonEmpty.:| [(P.Constructor "Nothing" [], E.Var "default")]))) ]

evaluatesToInt :: E.Expr -> Int -> Expectation
evaluatesToInt expr val = case eval defs expr of
    Just (V.Int n) -> n `shouldBe` val
    _ -> expectationFailure $ show expr ++ " should evaluate to Int " ++ show val
