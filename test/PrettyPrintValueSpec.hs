module PrettyPrintValueSpec where

import Test.Hspec
import PrettyPrintValue
import qualified Value as V

spec :: Spec
spec = it "prints values" $ do
    V.Int 1 `prints` "1"
    V.Constructor "Just" [V.Int 1] `prints` "Just 1"
    V.Constructor "Just" [V.Constructor "True" []] `prints` "Just True"
    V.Constructor "Just" [V.Constructor "Just" [V.Int 1]] `prints` "Just (Just 1)"

prints :: V.Value -> String -> Expectation
prints = shouldBe . prettyPrintValue
