import Data.Maybe (Maybe (..))
import Calculus (Var, Expr (..), Type (..), Val (..), eval, inferType)

main :: IO ()
main = do
    putStrLn ""
    checkEval one (IntVal 1)
    checkEval two (IntVal 2)
    checkEval (Call (identity "x") one) (IntVal 1)
    checkEval (Call (const1 "x") two) (IntVal 1)
    checkEval (Call (Call (constFn "x" "y") two) one) (IntVal 2)
    checkType one IntType
    checkType (identity "x") (FnType "x" $ VarType "x")
    checkType (Call (identity "x") one) IntType
    checkType (const1 "x") (FnType "x" IntType)
    checkType (Call (const1 "x") two) IntType
    checkType (constFn "x" "y") (FnType "x" . FnType "y" $ VarType "x")
    checkType (Call (constFn "x" "y") two) (FnType "y" IntType)
    checkType (Call (Call (constFn "x" "y") two) one) IntType

checkEval :: Expr -> Val -> IO ()
checkEval expr expectedVal = print $ case (eval expr, expectedVal) of
    (Just (IntVal n), IntVal m) -> n == m
    _ -> False

checkType :: Expr -> Type -> IO ()
checkType expr expectedType = print $ inferType expr == Just expectedType

one :: Expr
one = IntExpr 1

two :: Expr
two = IntExpr 2

identity :: Var -> Expr
identity var = FnExpr var $ VarExpr var

const1 :: Var -> Expr
const1 var = FnExpr var one

constFn :: Var -> Var -> Expr
constFn var1 var2 = FnExpr var1 . FnExpr var2 $ VarExpr var1
