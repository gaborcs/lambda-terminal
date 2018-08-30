module PrettyPrintSpec where

import Test.Hspec
import PrettyPrint
import qualified Type as T

spec :: Spec
spec = it "handles types" $ do
    T.Int `prints` "Int"
    T.Var 0 `prints` "a"
    T.Var 1 `prints` "b"
    T.Fn T.Int T.Int `prints` "Int -> Int"
    T.Fn T.Int (T.Fn T.Int T.Int) `prints` "Int -> Int -> Int"
    T.Fn (T.Fn T.Int T.Int) T.Int `prints` "(Int -> Int) -> Int"

prints :: T.Type -> String -> Expectation
prints = shouldBe . prettyPrint
