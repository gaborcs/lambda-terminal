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
    E.Integer 1 `evaluatesToInteger` 1
    E.Integer 2 `evaluatesToInteger` 2
    E.Call (E.Def "id") (E.Integer 1) `evaluatesToInteger` 1
    E.Call (E.Def "constOne") (E.Integer 2) `evaluatesToInteger` 1
    E.Call (E.Call (E.Def "const") (E.Integer 1)) (E.Integer 2) `evaluatesToInteger` 1
    E.Call (E.Def "inc") (E.Integer 2) `evaluatesToInteger` 3
    E.Call (E.Def "boolToInteger") (E.Constructor FalseC) `evaluatesToInteger` 0
    E.Call (E.Def "boolToInteger") (E.Constructor TrueC) `evaluatesToInteger` 1
    E.Call (E.Def "integerToBool") (E.Integer 0) `evaluatesToBool` FalseC
    E.Call (E.Def "integerToBool") (E.Integer 1) `evaluatesToBool` TrueC
    E.Call (E.Call (E.Def "fromMaybe") (E.Integer 0)) (E.Constructor NothingC) `evaluatesToInteger` 0
    E.Call (E.Call (E.Def "fromMaybe") (E.Integer 0)) (E.Call (E.Constructor JustC) (E.Integer 1)) `evaluatesToInteger` 1

defs :: Map.Map String (E.Expr String TestDataConstructorKey)
defs = Map.fromList
    [ ("id", E.fn "x" $ E.Var "x")
    , ("constOne", E.fn "x" $ E.Integer 1)
    , ("const", E.fn "x" . E.fn "y" $ E.Var "x")
    , ("inc", E.Call (E.Primitive Plus) (E.Integer 1))
    , ("boolToInteger", E.Fn ((P.Constructor FalseC [], E.Integer 0) NonEmpty.:| [(P.Constructor TrueC [], E.Integer 1)]))
    , ("integerToBool", E.Fn ((P.Integer 0, E.Constructor FalseC) NonEmpty.:| [(P.Wildcard, E.Constructor TrueC)]))
    , ("fromMaybe", E.fn "default" (E.Fn ((P.Constructor JustC [P.Var "x"], E.Var "x") NonEmpty.:| [(P.Constructor NothingC [], E.Var "default")])))
    ]

evaluatesToInteger :: E.Expr String TestDataConstructorKey -> Integer -> Expectation
evaluatesToInteger expr val = case eval defs expr of
    Just (V.Integer n) -> n `shouldBe` val
    _ -> expectationFailure $ show expr ++ " should evaluate to Integer " ++ show val

evaluatesToBool :: E.Expr String TestDataConstructorKey -> TestDataConstructorKey -> Expectation
evaluatesToBool expr dataConstructorKey = case eval defs expr of
    Just (V.Constructor key []) -> key `shouldBe` dataConstructorKey
    _ -> expectationFailure $ show expr ++ " should evaluate to " ++ show dataConstructorKey
