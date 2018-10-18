module EvalSpec where

import Test.Hspec
import Eval
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified BuiltInPrimitives as BP
import qualified BuiltInTypes as BT
import qualified Expr as E
import qualified Pattern as P
import qualified Type as T
import qualified Value as V

spec :: Spec
spec = it "evaluates expressions" $ do
    E.Int 1 `evaluatesToInt` 1
    E.Int 2 `evaluatesToInt` 2
    E.Call (E.Def "id") (E.Int 1) `evaluatesToInt` 1
    E.Call (E.Def "constOne") (E.Int 2) `evaluatesToInt` 1
    E.Call (E.Call (E.Def "const") (E.Int 1)) (E.Int 2) `evaluatesToInt` 1
    E.Call (E.Def "inc") (E.Int 2) `evaluatesToInt` 3
    E.Call (E.Def "boolToInt") (E.Constructor (T.DataConstructorKey BT.Bool "False")) `evaluatesToInt` 0
    E.Call (E.Def "boolToInt") (E.Constructor (T.DataConstructorKey BT.Bool "True")) `evaluatesToInt` 1
    E.Call (E.Def "intToBool") (E.Int 0) `evaluatesToBool` False
    E.Call (E.Def "intToBool") (E.Int 1) `evaluatesToBool` True
    E.Call (E.Call (E.Def "fromMaybe") (E.Int 0)) (E.Constructor (T.DataConstructorKey BT.Maybe "Nothing")) `evaluatesToInt` 0
    E.Call (E.Call (E.Def "fromMaybe") (E.Int 0)) (E.Call (E.Constructor (T.DataConstructorKey BT.Maybe "Just")) (E.Int 1)) `evaluatesToInt` 1

defs :: Map.Map String (E.Expr String BT.DataConstructorKey BP.Primitive)
defs = Map.fromList
    [ ("id", E.fn "x" $ E.Var "x")
    , ("constOne", E.fn "x" $ E.Int 1)
    , ("const", E.fn "x" . E.fn "y" $ E.Var "x")
    , ("inc", E.Call (E.Primitive BP.Plus) (E.Int 1))
    , ("boolToInt", E.Fn ((P.Constructor (T.DataConstructorKey BT.Bool "False") [], E.Int 0) NonEmpty.:|
        [(P.Constructor (T.DataConstructorKey BT.Bool "True") [], E.Int 1)]))
    , ("intToBool", E.Fn ((P.Int 0, E.Constructor (T.DataConstructorKey BT.Bool "False")) NonEmpty.:|
        [(P.Wildcard, E.Constructor (T.DataConstructorKey BT.Bool "True"))]))
    , ("fromMaybe", E.fn "default" (E.Fn ((P.Constructor (T.DataConstructorKey BT.Maybe "Just") [P.Var "x"], E.Var "x") NonEmpty.:|
        [(P.Constructor (T.DataConstructorKey BT.Maybe "Nothing") [], E.Var "default")]))) ]

evaluatesToInt :: E.Expr String BT.DataConstructorKey BP.Primitive -> Int -> Expectation
evaluatesToInt expr val = case eval defs expr of
    Just (V.Int n) -> n `shouldBe` val
    _ -> expectationFailure $ show expr ++ " should evaluate to Int " ++ show val

evaluatesToBool :: E.Expr String BT.DataConstructorKey BP.Primitive -> Bool -> Expectation
evaluatesToBool expr val = case eval defs expr of
    Just (V.Constructor (T.DataConstructorKey BT.Bool name) []) -> name `shouldBe` if val then "True" else "False"
    _ -> expectationFailure $ show expr ++ " should evaluate to " ++ show val
