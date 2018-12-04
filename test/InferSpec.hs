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

getConstructorType :: TestDataConstructorKey -> Maybe (T.Type Integer TestType)
getConstructorType key = Just $ case key of
    FalseC -> T.Constructor BoolT
    TrueC -> T.Constructor BoolT
    NothingC -> T.Call (T.Constructor MaybeT) (T.Var 0)
    JustC -> T.fn (T.Var 0) (T.Call (T.Constructor MaybeT) (T.Var 0))

spec :: Spec
spec = do
    it "infers the type of expressions" $ do
        E.Integer 1 `hasType` T.Integer
        E.Def "constOne" `hasType` T.fn (T.Var 0) T.Integer
        E.Def "id" `hasType` T.fn (T.Var 0) (T.Var 0)
        E.fn "x" (E.Def "constOne") `hasType` T.fn (T.Var 0) (T.fn (T.Var 1) T.Integer)
        E.Def "const" `hasType` T.fn (T.Var 0) (T.fn (T.Var 1) (T.Var 0))
        E.Call (E.Def "constOne") (E.Integer 2) `hasType` T.Integer
        E.Call (E.Def "id") (E.Integer 1) `hasType` T.Integer
        E.fn "f" (E.fn "a" (E.Call (E.Var "f") (E.Var "a"))) `hasType`
            T.fn (T.fn (T.Var 0) (T.Var 1)) (T.fn (T.Var 0) (T.Var 1))
        E.fn "a" (E.fn "f" (E.Call (E.Var "f") (E.Var "a"))) `hasType`
            T.fn (T.Var 0) (T.fn (T.fn (T.Var 0) (T.Var 1)) (T.Var 1))
        E.fn "f" (E.fn "g" (E.fn "a" (E.Call (E.Var "f") (E.Call (E.Var "g") (E.Var "a"))))) `hasType`
            T.fn (T.fn (T.Var 0) (T.Var 1)) (T.fn (T.fn (T.Var 2) (T.Var 0)) (T.fn (T.Var 2) (T.Var 1)))
        E.Def "diverge" `hasType` T.Var 0
        E.Def "divergeFn" `hasType` T.fn T.Integer (T.Var 0)
        E.Primitive Plus `hasType` T.fn T.Integer (T.fn T.Integer T.Integer)
        E.Def "inc" `hasType` T.fn T.Integer T.Integer
        E.Call (E.Def "inc") (E.Integer 1) `hasType` T.Integer
        E.Constructor NothingC `hasType` T.Call (T.Constructor MaybeT) (T.Var 0)
        E.Constructor JustC `hasType` T.fn (T.Var 0) (T.Call (T.Constructor MaybeT) (T.Var 0))
        E.Call (E.Constructor JustC) (E.Integer 1) `hasType` T.Call (T.Constructor MaybeT) T.Integer
        E.Def "integerToBool" `hasType` T.fn T.Integer (T.Constructor BoolT)
    it "indicates where type errors happen" $ do
        E.Call (E.Integer 1) (E.Integer 1) `failsAtPath` []
        E.fn "x" (E.Call (E.Integer 1) (E.Integer 1)) `failsAtPath` [1]
        E.Call (E.Def "inc") (E.Def "id") `failsAtPath` []

defs :: Map.Map String (E.Expr String TestDataConstructorKey)
defs = Map.fromList
    [ ("constOne", E.fn "x" $ E.Integer 1)
    , ("id", E.fn "x" $ E.Var "x")
    , ("const", E.fn "x" . E.fn "y" $ E.Var "x")
    , ("inc", E.Call (E.Primitive Plus) $ E.Integer 1)
    , ("diverge", E.Def "diverge")
    , ("divergeFn", E.fn "n" $ E.Call (E.Def "divergeFn") (E.Integer 1))
    , ("integerToBool", E.Fn ((P.Integer 0, E.Constructor FalseC) NonEmpty.:| [(P.Wildcard, E.Constructor TrueC)]))
    ]

hasType :: E.Expr String TestDataConstructorKey -> T.Type Int TestType -> Expectation
hasType expr expectedType = case inferType getConstructorType defs expr of
    Typed (TypeTree actualType _) -> indexTVarsFromZero actualType `shouldBe` expectedType
    _ -> expectationFailure $ "type error for: " ++ show expr

indexTVarsFromZero :: T.Type Int t -> T.Type Int t
indexTVarsFromZero t = apply (Map.fromList $ zip (T.getTypeVars t) (T.Var <$> [0..])) t

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
