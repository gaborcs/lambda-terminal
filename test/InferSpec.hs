module InferSpec where

import Test.Hspec
import Infer
import Primitive
import Util
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Expr as E
import qualified Pattern as P
import qualified Type as T

type Path = [ChildIndex]
type ChildIndex = Int
data TestType = BoolT | MaybeT deriving (Eq, Show)
data TestDataConstructorKey = FalseC | TrueC | NothingC | JustC deriving (Eq, Show)

getConstructorType :: TestDataConstructorKey -> Maybe (T.Type TestType)
getConstructorType key = Just $ case key of
    FalseC -> T.Constructor BoolT []
    TrueC -> T.Constructor BoolT []
    NothingC -> T.Constructor MaybeT [T.Var 0]
    JustC -> T.Fn (T.Var 0) (T.Constructor MaybeT [T.Var 0])

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
        E.Primitive Plus `hasType` T.Fn T.Int (T.Fn T.Int T.Int)
        E.Def "inc" `hasType` T.Fn T.Int T.Int
        E.Call (E.Def "inc") (E.Int 1) `hasType` T.Int
        E.Constructor NothingC `hasType` T.Constructor MaybeT [T.Var 0]
        E.Constructor JustC `hasType` T.Fn (T.Var 0) (T.Constructor MaybeT [T.Var 0])
        E.Call (E.Constructor JustC) (E.Int 1) `hasType` T.Constructor MaybeT [T.Int]
        E.Def "intToBool" `hasType` T.Fn T.Int (T.Constructor BoolT [])
    it "indicates where type errors happen" $ do
        E.Call (E.Int 1) (E.Int 1) `failsAtPath` []
        E.fn "x" (E.Call (E.Int 1) (E.Int 1)) `failsAtPath` [1]
        E.Call (E.Def "inc") (E.Def "id") `failsAtPath` []

defs :: Map.Map String (E.Expr String TestDataConstructorKey)
defs = Map.fromList
    [ ("constOne", E.fn "x" $ E.Int 1)
    , ("id", E.fn "x" $ E.Var "x")
    , ("const", E.fn "x" . E.fn "y" $ E.Var "x")
    , ("inc", E.Call (E.Primitive Plus) $ E.Int 1)
    , ("diverge", E.Def "diverge")
    , ("divergeFn", E.fn "n" $ E.Call (E.Def "divergeFn") (E.Int 1))
    , ("intToBool", E.Fn ((P.Int 0, E.Constructor FalseC) NonEmpty.:| [(P.Wildcard, E.Constructor TrueC)]))
    ]

hasType :: E.Expr String TestDataConstructorKey -> T.Type TestType -> Expectation
hasType expr expectedType = case inferType getConstructorType defs expr of
    Typed (TypeTree actualType _) -> indexTVarsFromZero actualType `shouldBe` expectedType
    _ -> expectationFailure $ "type error for: " ++ show expr

indexTVarsFromZero :: T.Type t -> T.Type t
indexTVarsFromZero t = apply (Map.fromList $ zip (typeVars t) (T.Var <$> [0..])) t

failsAtPath :: E.Expr String TestDataConstructorKey -> Path -> Expectation
failsAtPath expr path = inferType getConstructorType defs expr `hasErrorAtPath` path

hasErrorAtPath :: InferResult TestType -> Path -> Expectation
hasErrorAtPath inferResult path = case inferResult of
    Typed _ -> expectationFailure "should have a type error"
    TypeError childResults -> case path of
        [] -> childResults `shouldSatisfy` hasErrorAtRoot
        index:restOfPath -> case getItemAtIndex index childResults of
            Just childResult -> childResult `hasErrorAtPath` restOfPath
            Nothing -> expectationFailure "path doesn't exist"
