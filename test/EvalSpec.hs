module EvalSpec where

import Test.Hspec
import Eval
import Primitive
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Expr as E
import qualified Pattern as P
import qualified Value as V

data TestDataConstructorKey = FalseC | TrueC | NothingC | JustC deriving (Eq, Show)

spec :: Spec
spec = it "evaluates expressions" $ do
    E.Int 1 `evaluatesToInt` 1
    E.Int 2 `evaluatesToInt` 2
    E.Call (E.Def "id") (E.Int 1) `evaluatesToInt` 1
    E.Call (E.Def "constOne") (E.Int 2) `evaluatesToInt` 1
    E.Call (E.Call (E.Def "const") (E.Int 1)) (E.Int 2) `evaluatesToInt` 1
    E.Call (E.Def "inc") (E.Int 2) `evaluatesToInt` 3
    E.Call (E.Def "boolToInt") (E.Constructor FalseC) `evaluatesToInt` 0
    E.Call (E.Def "boolToInt") (E.Constructor TrueC) `evaluatesToInt` 1
    E.Call (E.Def "intToBool") (E.Int 0) `evaluatesToBool` FalseC
    E.Call (E.Def "intToBool") (E.Int 1) `evaluatesToBool` TrueC
    E.Call (E.Call (E.Def "fromMaybe") (E.Int 0)) (E.Constructor NothingC) `evaluatesToInt` 0
    E.Call (E.Call (E.Def "fromMaybe") (E.Int 0)) (E.Call (E.Constructor JustC) (E.Int 1)) `evaluatesToInt` 1

defs :: Map.Map String (E.Expr String TestDataConstructorKey)
defs = Map.fromList
    [ ("id", E.fn "x" $ E.Var "x")
    , ("constOne", E.fn "x" $ E.Int 1)
    , ("const", E.fn "x" . E.fn "y" $ E.Var "x")
    , ("inc", E.Call (E.Primitive Plus) (E.Int 1))
    , ("boolToInt", E.Fn ((P.Constructor FalseC [], E.Int 0) NonEmpty.:| [(P.Constructor TrueC [], E.Int 1)]))
    , ("intToBool", E.Fn ((P.Int 0, E.Constructor FalseC) NonEmpty.:| [(P.Wildcard, E.Constructor TrueC)]))
    , ("fromMaybe", E.fn "default" (E.Fn ((P.Constructor JustC [P.Var "x"], E.Var "x") NonEmpty.:| [(P.Constructor NothingC [], E.Var "default")])))
    ]

evaluatesToInt :: E.Expr String TestDataConstructorKey -> Int -> Expectation
evaluatesToInt expr val = case eval defs expr of
    Just (V.Int n) -> n `shouldBe` val
    _ -> expectationFailure $ show expr ++ " should evaluate to Int " ++ show val

evaluatesToBool :: E.Expr String TestDataConstructorKey -> TestDataConstructorKey -> Expectation
evaluatesToBool expr dataConstructorKey = case eval defs expr of
    Just (V.Constructor key []) -> key `shouldBe` dataConstructorKey
    _ -> expectationFailure $ show expr ++ " should evaluate to " ++ show dataConstructorKey
