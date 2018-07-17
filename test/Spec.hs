import Calculus
import qualified Data.Map as Map

main :: IO ()
main = do
    putStrLn ""
    checkEval one (IntVal 1)
    checkEval two (IntVal 2)
    checkEval (Call (VarExpr "identity") one) (IntVal 1)
    checkEval (Call (const1 "x") two) (IntVal 1)
    checkEval (Call (Call (VarExpr "const") two) one) (IntVal 2)
    checkType one IntType
    checkType (VarExpr "identity") (FnType "x" $ VarType "x")
    checkType (Call (VarExpr "identity") one) IntType
    checkType (const1 "x") (FnType "x" IntType)
    checkType (Call (const1 "x") two) IntType
    checkType (VarExpr "const") (FnType "x" . FnType "y" $ VarType "x")
    checkType (Call (VarExpr "const") two) (FnType "y" IntType)
    checkType (Call (Call (VarExpr "const") two) one) IntType
    checkTypeError (Call (VarExpr "identity") (Call one one)) (ArgError CalleeNotFn)
    checkTypeError (Call one one) CalleeNotFn
    checkTypeError (Call one (Call one one)) (CalleeNotFnAndArgError CalleeNotFn)
    checkTypeError (Call (Call one one) one) (CalleeError CalleeNotFn)
    checkTypeError (Call (Call one one) (Call one one)) (CalleeAndArgError CalleeNotFn CalleeNotFn)
    checkTypeError (Call (VarExpr "identity") (Call (Call one one) one)) (ArgError (CalleeError CalleeNotFn))

namedExprs :: Map.Map Var Expr
namedExprs = Map.fromList $
    [ ("identity", identity "x")
    , ("const", constFn "x" "y") ]

namedExprVals :: Map.Map Var (Maybe Val)
namedExprVals = evalExprs namedExprs

namedExprTypes :: Map.Map Var (Either ErrorTree Type)
namedExprTypes = inferTypes namedExprs

checkEval :: Expr -> Val -> IO ()
checkEval expr expectedVal = print $ case (evalUnderEnv namedExprVals expr, expectedVal) of
    (Just (IntVal n), IntVal m) -> n == m
    _ -> False

checkType :: Expr -> Type -> IO ()
checkType expr expectedType = print $ inferTypeUnderEnv namedExprTypes expr == Right expectedType

checkTypeError :: Expr -> ErrorTree -> IO ()
checkTypeError expr errorTree = print $ inferTypeUnderEnv namedExprTypes expr == Left errorTree

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
