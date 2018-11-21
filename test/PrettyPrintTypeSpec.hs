module PrettyPrintTypeSpec where

import Test.Hspec
import PrettyPrintType
import qualified Type as T

spec :: Spec
spec = it "prints types" $ do
    T.Int `prints` "Int"
    T.Var "a" `prints` "a"
    T.Var "b" `prints` "b"
    T.fn T.Int T.Int `prints` "λ Int Int"
    T.fn T.Int (T.fn T.Int T.Int) `prints` "λ Int (λ Int Int)"
    T.fn (T.fn T.Int T.Int) T.Int `prints` "λ (λ Int Int) Int"
    T.Constructor "Bool" `prints` "Bool"
    T.Call (T.Constructor "Maybe") (T.Var "a") `prints` "Maybe a"
    T.Call (T.Call (T.Constructor "Either") (T.Var "a")) (T.Var "b") `prints` "Either a b"
    T.Call (T.Constructor "Maybe") (T.fn T.Int T.Int) `prints` "Maybe (λ Int Int)"
    T.fn (T.Call (T.Constructor "Maybe") (T.Var "a")) T.Int `prints` "λ (Maybe a) Int"
    T.fn (T.Call (T.Constructor "Maybe") (T.fn T.Int T.Int)) T.Int `prints` "λ (Maybe (λ Int Int)) Int"
    T.Call (T.Constructor "Maybe") (T.Constructor "Bool") `prints` "Maybe Bool"
    T.Call (T.Constructor "Maybe") (T.Call (T.Constructor "Maybe") (T.Var "a")) `prints` "Maybe (Maybe a)"

prints :: T.Type String String -> String -> Expectation
prints = shouldBe . prettyPrintType id
