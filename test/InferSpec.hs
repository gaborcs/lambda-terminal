module InferSpec where

import Test.Hspec
import Infer
import qualified Data.Map as Map
import qualified Expr as E
import qualified Type as T

spec :: Spec
spec = do
    it "infers the type of expressions" $ do
        E.Int 1 `hasType` T.Int
        E.Ref "constOne" `hasType` T.Fn (T.Var 0) T.Int
        E.Ref "id" `hasType` T.Fn (T.Var 0) (T.Var 0)
        E.Fn "x" (E.Ref "constOne") `hasType` T.Fn (T.Var 0) (T.Fn (T.Var 1) T.Int)
        E.Ref "const" `hasType` T.Fn (T.Var 0) (T.Fn (T.Var 1) (T.Var 0))
        E.Call (E.Ref "constOne") (E.Int 2) `hasType` T.Int
        E.Call (E.Ref "id") (E.Int 1) `hasType` T.Int
        E.Fn "f" (E.Fn "a" (E.Call (E.Var "f") (E.Var "a"))) `hasType`
            T.Fn (T.Fn (T.Var 0) (T.Var 1)) (T.Fn (T.Var 0) (T.Var 1))
        E.Fn "a" (E.Fn "f" (E.Call (E.Var "f") (E.Var "a"))) `hasType`
            T.Fn (T.Var 0) (T.Fn (T.Fn (T.Var 0) (T.Var 1)) (T.Var 1))
        E.Fn "f" (E.Fn "g" (E.Fn "a" (E.Call (E.Var "f") (E.Call (E.Var "g") (E.Var "a"))))) `hasType`
            T.Fn (T.Fn (T.Var 0) (T.Var 1)) (T.Fn (T.Fn (T.Var 2) (T.Var 0)) (T.Fn (T.Var 2) (T.Var 1)))
        E.Ref "diverge" `hasType` T.Var 0
        E.Ref "divergeFn" `hasType` T.Fn T.Int (T.Var 0)
        E.Plus `hasType` T.Fn T.Int (T.Fn T.Int T.Int)
        E.Ref "inc" `hasType` T.Fn T.Int T.Int
        E.Call (E.Ref "inc") (E.Int 1) `hasType` T.Int
    it "indicates where type errors happen" $ do
        E.Var "unbound" `failsAtPath` []
        E.Fn "x" (E.Var "unbound") `failsAtPath` [0]
        E.Call (E.Int 1) (E.Int 1) `failsAtPath` []
        E.Call (E.Var "unbound") (E.Int 1) `failsAtPath` [0]
        E.Call (E.Int 1) (E.Var "unbound") `failsAtPath` [1]
        E.Ref "unboundVar" `failsAtPath` []
        E.Call (E.Ref "inc") (E.Ref "id") `failsAtPath` []

defs :: Map.Map E.ExprName E.Expr
defs = Map.fromList
    [ ("constOne", E.Fn "x" $ E.Int 1)
    , ("id", E.Fn "x" $ E.Var "x")
    , ("const", E.Fn "x" . E.Fn "y" $ E.Var "x")
    , ("inc", E.Call E.Plus $ E.Int 1)
    , ("diverge", E.Ref "diverge")
    , ("divergeFn", E.Fn "n" $ E.Call (E.Ref "divergeFn") (E.Int 1))
    , ("unboundVar", E.Var "unbound") ]

hasType :: E.Expr -> T.Type -> Expectation
hasType expr t = indexTVarsFromZero <$> maybeType `shouldBe` Just t where
    (_, maybeType) = inferType defs expr []

indexTVarsFromZero :: T.Type -> T.Type
indexTVarsFromZero t = apply (Map.fromList $ zip (typeVars t) (T.Var <$> [0..])) t

failsAtPath :: E.Expr -> ErrorPath -> Expectation
failsAtPath expr errorPath = maybeErrorPath `shouldBe` Just errorPath where
    (maybeErrorPath, _) = inferType defs expr []
