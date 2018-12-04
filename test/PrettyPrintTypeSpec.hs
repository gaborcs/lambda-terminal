module PrettyPrintTypeSpec where

import Test.Hspec
import PrettyPrintType
import qualified Type as T

spec :: Spec
spec = it "prints types" $ do
    T.Integer `prints` "Integer"
    T.Var "a" `prints` "a"
    T.Var "b" `prints` "b"
    T.fn T.Integer T.Integer `prints` "λ Integer Integer"
    T.fn T.Integer (T.fn T.Integer T.Integer) `prints` "λ Integer (λ Integer Integer)"
    T.fn (T.fn T.Integer T.Integer) T.Integer `prints` "λ (λ Integer Integer) Integer"
    T.Constructor "Bool" `prints` "Bool"
    T.Call (T.Constructor "Maybe") (T.Var "a") `prints` "Maybe a"
    T.Call (T.Call (T.Constructor "Either") (T.Var "a")) (T.Var "b") `prints` "Either a b"
    T.Call (T.Constructor "Maybe") (T.fn T.Integer T.Integer) `prints` "Maybe (λ Integer Integer)"
    T.fn (T.Call (T.Constructor "Maybe") (T.Var "a")) T.Integer `prints` "λ (Maybe a) Integer"
    T.fn (T.Call (T.Constructor "Maybe") (T.fn T.Integer T.Integer)) T.Integer `prints` "λ (Maybe (λ Integer Integer)) Integer"
    T.Call (T.Constructor "Maybe") (T.Constructor "Bool") `prints` "Maybe Bool"
    T.Call (T.Constructor "Maybe") (T.Call (T.Constructor "Maybe") (T.Var "a")) `prints` "Maybe (Maybe a)"

prints :: T.Type String String -> String -> Expectation
prints = shouldBe . prettyPrintType id
