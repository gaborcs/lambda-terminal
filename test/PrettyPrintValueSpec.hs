module PrettyPrintValueSpec where

import Test.Hspec
import PrettyPrintValue
import qualified Expr as E
import qualified Value as V

spec :: Spec
spec = it "prints values" $ do
    V.Int 1 `prints` "1"
    constructor "Just" [V.Int 1] `prints` "Just 1"
    constructor "Just" [constructor "True" []] `prints` "Just True"
    constructor "Just" [constructor "Just" [V.Int 1]] `prints` "Just (Just 1)"

prints :: V.Value String -> String -> Expectation
prints v s = prettyPrintValue id v `shouldBe` Just s

constructor :: String -> [V.Value String] -> V.Value String
constructor name values = V.Constructor name $ fmap Just values
