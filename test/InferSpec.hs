module InferSpec where

import Test.Hspec
import Infer
import Util
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified BuiltInPrimitives
import qualified Expr as E
import qualified Pattern as P
import qualified Type as T

spec :: Spec
spec = do
    it "infers the type of expressions" $ do
        E.Int 1 `hasType` T.Int
        E.Def "constOne" `hasType` T.Fn (T.Var 0) T.Int
        E.Def "id" `hasType` T.Fn (T.Var 0) (T.Var 0)
        E.fn "x" (E.Def "constOne") `hasType` T.Fn (T.Var 0) (T.Fn (T.Var 1) T.Int)
        E.Def "const" `hasType` T.Fn (T.Var 0) (T.Fn (T.Var 1) (T.Var 0))
        E.Call (E.Def "constOne") (E.Int 2) `hasType` T.Int
        E.Call (E.Def "id") (E.Int 1) `hasType` T.Int
        E.fn "f" (E.fn "a" (E.Call (E.Var "f") (E.Var "a"))) `hasType`
            T.Fn (T.Fn (T.Var 0) (T.Var 1)) (T.Fn (T.Var 0) (T.Var 1))
        E.fn "a" (E.fn "f" (E.Call (E.Var "f") (E.Var "a"))) `hasType`
            T.Fn (T.Var 0) (T.Fn (T.Fn (T.Var 0) (T.Var 1)) (T.Var 1))
        E.fn "f" (E.fn "g" (E.fn "a" (E.Call (E.Var "f") (E.Call (E.Var "g") (E.Var "a"))))) `hasType`
            T.Fn (T.Fn (T.Var 0) (T.Var 1)) (T.Fn (T.Fn (T.Var 2) (T.Var 0)) (T.Fn (T.Var 2) (T.Var 1)))
        E.Def "diverge" `hasType` T.Var 0
        E.Def "divergeFn" `hasType` T.Fn T.Int (T.Var 0)
        E.Primitive BuiltInPrimitives.Plus `hasType` T.Fn T.Int (T.Fn T.Int T.Int)
        E.Def "inc" `hasType` T.Fn T.Int T.Int
        E.Call (E.Def "inc") (E.Int 1) `hasType` T.Int
        E.Constructor "Nothing" `hasType` T.Constructor "Maybe" [T.Var 0]
        E.Constructor "Just" `hasType` T.Fn (T.Var 0) (T.Constructor "Maybe" [T.Var 0])
        E.Call (E.Constructor "Just") (E.Int 1) `hasType` T.Constructor "Maybe" [T.Int]
        E.Def "intToBool" `hasType` T.Fn T.Int (T.Constructor "Bool" [])
    it "indicates where type errors happen" $ do
        E.Call (E.Int 1) (E.Int 1) `failsAtPath` []
        E.fn "x" (E.Call (E.Int 1) (E.Int 1)) `failsAtPath` [1]
        E.Call (E.Def "inc") (E.Def "id") `failsAtPath` []

constructorTypes :: Map.Map E.ConstructorName T.Type
constructorTypes = Map.fromList
    [ ("False", T.Constructor "Bool" [])
    , ("True", T.Constructor "Bool" [])
    , ("Nothing", T.Constructor "Maybe" [T.Var 0])
    , ("Just", T.Fn (T.Var 0) (T.Constructor "Maybe" [T.Var 0])) ]

defs :: Map.Map String (E.Expr String BuiltInPrimitives.Primitive)
defs = Map.fromList
    [ ("constOne", E.fn "x" $ E.Int 1)
    , ("id", E.fn "x" $ E.Var "x")
    , ("const", E.fn "x" . E.fn "y" $ E.Var "x")
    , ("inc", E.Call (E.Primitive BuiltInPrimitives.Plus) $ E.Int 1)
    , ("diverge", E.Def "diverge")
    , ("divergeFn", E.fn "n" $ E.Call (E.Def "divergeFn") (E.Int 1))
    , ("intToBool", E.Fn ((P.Int 0, E.Constructor "False") NonEmpty.:| [(P.Wildcard, E.Constructor "True")])) ]

hasType :: E.Expr String BuiltInPrimitives.Primitive -> T.Type -> Expectation
hasType expr expectedType = case inferType constructorTypes defs expr of
    Typed (TypeTree actualType _) -> indexTVarsFromZero actualType `shouldBe` expectedType
    _ -> expectationFailure $ "type error for: " ++ show expr

indexTVarsFromZero :: T.Type -> T.Type
indexTVarsFromZero t = apply (Map.fromList $ zip (typeVars t) (T.Var <$> [0..])) t

failsAtPath :: E.Expr String BuiltInPrimitives.Primitive -> E.Path -> Expectation
failsAtPath expr path = inferType constructorTypes defs expr `hasErrorAtPath` path

hasErrorAtPath :: InferResult -> E.Path -> Expectation
hasErrorAtPath inferResult path = case inferResult of
    Typed _ -> expectationFailure "should have a type error"
    TypeError childResults -> case path of
        [] -> childResults `shouldSatisfy` hasErrorAtRoot
        index:restOfPath -> case getItemAtIndex index childResults of
            Just childResult -> childResult `hasErrorAtPath` restOfPath
            Nothing -> expectationFailure "path doesn't exist"
