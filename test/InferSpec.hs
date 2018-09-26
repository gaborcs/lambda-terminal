module InferSpec where

import Test.Hspec
import Infer
import Primitive
import Util
import qualified Data.Map as Map
import qualified Expr as E
import qualified Type as T

spec :: Spec
spec = do
    it "infers the type of expressions" $ do
        E.Int 1 `hasType` T.Int
        E.Ref "constOne" `hasType` T.Fn (T.Var 0) T.Int
        E.Ref "id" `hasType` T.Fn (T.Var 0) (T.Var 0)
        E.fn "x" (E.Ref "constOne") `hasType` T.Fn (T.Var 0) (T.Fn (T.Var 1) T.Int)
        E.Ref "const" `hasType` T.Fn (T.Var 0) (T.Fn (T.Var 1) (T.Var 0))
        E.Call (E.Ref "constOne") (E.Int 2) `hasType` T.Int
        E.Call (E.Ref "id") (E.Int 1) `hasType` T.Int
        E.fn "f" (E.fn "a" (E.Call (E.Var "f") (E.Var "a"))) `hasType`
            T.Fn (T.Fn (T.Var 0) (T.Var 1)) (T.Fn (T.Var 0) (T.Var 1))
        E.fn "a" (E.fn "f" (E.Call (E.Var "f") (E.Var "a"))) `hasType`
            T.Fn (T.Var 0) (T.Fn (T.Fn (T.Var 0) (T.Var 1)) (T.Var 1))
        E.fn "f" (E.fn "g" (E.fn "a" (E.Call (E.Var "f") (E.Call (E.Var "g") (E.Var "a"))))) `hasType`
            T.Fn (T.Fn (T.Var 0) (T.Var 1)) (T.Fn (T.Fn (T.Var 2) (T.Var 0)) (T.Fn (T.Var 2) (T.Var 1)))
        E.Ref "diverge" `hasType` T.Var 0
        E.Ref "divergeFn" `hasType` T.Fn T.Int (T.Var 0)
        E.Primitive Plus `hasType` T.Fn T.Int (T.Fn T.Int T.Int)
        E.Ref "inc" `hasType` T.Fn T.Int T.Int
        E.Call (E.Ref "inc") (E.Int 1) `hasType` T.Int
        E.Constructor "Nothing" `hasType` T.Constructor "Maybe" [T.Var 0]
        E.Constructor "Just" `hasType` T.Fn (T.Var 0) (T.Constructor "Maybe" [T.Var 0])
        E.Call (E.Constructor "Just") (E.Int 1) `hasType` T.Constructor "Maybe" [T.Int]
    it "indicates where type errors happen" $ do
        E.Call (E.Int 1) (E.Int 1) `failsAtPath` []
        E.fn "x" (E.Call (E.Int 1) (E.Int 1)) `failsAtPath` [1]
        E.Call (E.Ref "inc") (E.Ref "id") `failsAtPath` []

constructorTypes :: Map.Map E.ConstructorName T.Type
constructorTypes = Map.fromList
    [ ("Nothing", T.Constructor "Maybe" [T.Var 0])
    , ("Just", T.Fn (T.Var 0) (T.Constructor "Maybe" [T.Var 0])) ]

defs :: Map.Map E.ExprName E.Expr
defs = Map.fromList
    [ ("constOne", E.fn "x" $ E.Int 1)
    , ("id", E.fn "x" $ E.Var "x")
    , ("const", E.fn "x" . E.fn "y" $ E.Var "x")
    , ("inc", E.Call (E.Primitive Plus) $ E.Int 1)
    , ("diverge", E.Ref "diverge")
    , ("divergeFn", E.fn "n" $ E.Call (E.Ref "divergeFn") (E.Int 1)) ]

hasType :: E.Expr -> T.Type -> Expectation
hasType expr expectedType = case inferType constructorTypes defs expr of
    Typed (TypeTree actualType _) -> indexTVarsFromZero actualType `shouldBe` expectedType
    _ -> expectationFailure $ "type error for: " ++ show expr

indexTVarsFromZero :: T.Type -> T.Type
indexTVarsFromZero t = apply (Map.fromList $ zip (typeVars t) (T.Var <$> [0..])) t

failsAtPath :: E.Expr -> E.Path -> Expectation
failsAtPath expr path = inferType constructorTypes defs expr `hasErrorAtPath` path

hasErrorAtPath :: InferResult -> E.Path -> Expectation
hasErrorAtPath inferResult path = case inferResult of
    Typed _ -> expectationFailure "should have a type error"
    TypeError childResults -> case path of
        [] -> childResults `shouldSatisfy` hasErrorAtRoot
        index:restOfPath -> case getItemAtIndex childResults index of
            Just childResult -> childResult `hasErrorAtPath` restOfPath
            Nothing -> expectationFailure "path doesn't exist"
