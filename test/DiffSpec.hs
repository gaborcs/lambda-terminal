module DiffSpec where

import Data.List.NonEmpty
import Test.Hspec
import Diff
import Expr
import qualified Pattern as P

diff :: Expr Int Int -> Expr Int Int -> Maybe Path
diff = getDiffPathBetweenExprs

spec :: Spec
spec = it "checks where two expressions are different" $ do
    diff (Call Hole Hole) (Call Hole Hole) `shouldBe` Nothing
    diff (Call (Def 0) Hole) (Call Hole Hole) `shouldBe` Just [0]
    diff (Call Hole Hole) (Call Hole (Def 0)) `shouldBe` Just [1]
    diff (fn "x" Hole) (fn "x" Hole) `shouldBe` Nothing
    diff (fn "x" Hole) (fn "y" Hole) `shouldBe` Just [0]
    diff (fn "x" Hole) (fn "x" (Def 0)) `shouldBe` Just [1]
    diff (fn "x" Hole) (fn "y" (Def 0)) `shouldBe` Just []
    diff (Fn $ (P.Wildcard, Hole) :| [(P.Wildcard, Hole)]) (Fn $ (P.Wildcard, Hole) :| [(P.Wildcard, Hole)]) `shouldBe` Nothing
    diff (Fn $ (P.Wildcard, Hole) :| [(P.Var "x", Hole)]) (Fn $ (P.Wildcard, Hole) :| [(P.Wildcard, Hole)]) `shouldBe` Just [2]
    diff (Fn $ (P.Wildcard, Hole) :| [(P.Wildcard, Var "x")]) (Fn $ (P.Wildcard, Hole) :| [(P.Wildcard, Hole)]) `shouldBe` Just [3]
    diff (Fn $ (P.Wildcard, Hole) :| [(P.Var "x", Var "x")]) (Fn $ (P.Wildcard, Hole) :| [(P.Wildcard, Hole)]) `shouldBe` Just []
    diff (Fn $ (P.Wildcard, Hole) :| []) (Fn $ (P.Wildcard, Hole) :| [(P.Wildcard, Hole)]) `shouldBe` Just []
    diff (Fn $ (P.Wildcard, Hole) :| [(P.Wildcard, Hole)]) (Fn $ (P.Wildcard, Hole) :| []) `shouldBe` Just []
