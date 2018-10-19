module PrettyPrintTypeSpec where

import Test.Hspec
import PrettyPrintType
import qualified Type as T

spec :: Spec
spec = it "prints types" $ do
    T.Int `prints` "Int"
    T.Var 0 `prints` "a"
    T.Var 1 `prints` "b"
    T.Fn T.Int T.Int `prints` "Int -> Int"
    T.Fn T.Int (T.Fn T.Int T.Int) `prints` "Int -> Int -> Int"
    T.Fn (T.Fn T.Int T.Int) T.Int `prints` "(Int -> Int) -> Int"
    T.Constructor "Bool" [] `prints` "Bool"
    T.Constructor "Maybe" [T.Var 0] `prints` "Maybe a"
    T.Constructor "Either" [T.Var 0, T.Var 1] `prints` "Either a b"
    T.Constructor "Maybe" [T.Fn T.Int T.Int] `prints` "Maybe (Int -> Int)"
    T.Fn (T.Constructor "Maybe" [T.Var 0]) T.Int `prints` "Maybe a -> Int"
    T.Fn (T.Constructor "Maybe" [T.Fn T.Int T.Int]) T.Int `prints` "Maybe (Int -> Int) -> Int"
    T.Constructor "Maybe" [T.Constructor "Bool" []] `prints` "Maybe Bool"
    T.Constructor "Maybe" [T.Constructor "Maybe" [T.Var 0]] `prints` "Maybe (Maybe a)"

prints :: T.Type String -> String -> Expectation
prints = shouldBe . prettyPrintType id defaultTypeVarNames
