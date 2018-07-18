module Main where

import Data.Maybe
import Brick
import Graphics.Vty
import Calculus
import qualified Data.Map as Map

data State = State ExprName Path
type ExprName = String
type Path = [ChildIndex]
type ChildIndex = Int

main :: IO ()
main = do
    defaultMain app initialState
    return ()
    where initialState = State "main" []

app :: App State e String
app = App
    { appDraw = draw
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap = const attributeMap }

draw :: State -> [Widget n]
draw state = [ padBottom Max renderedExpr <=> str bottomStr ] where
    State exprName selectionPath = state
    expr = fromJust $ Map.lookup exprName namedExprs
    renderedExpr = renderExpr expr (Just selectionPath) maybeErrorTree
    maybeErrorTree = either Just (const Nothing) (inferTypeUnderEnv namedExprTypes expr)
    bottomStr = case (inferTypeUnderEnv namedExprTypes selectedExpr, evalUnderEnv namedExprVals selectedExpr) of
        (Right t, Just (IntVal v)) -> show v ++ ": " ++ show t
        (Right t, _) -> show t
        _ -> "Type error"
    selectedExpr = fromJust $ getSubExprAtPath expr selectionPath

renderExpr :: Expr -> Maybe Path -> Maybe ErrorTree -> Widget n
renderExpr expr maybeSelectionPath maybeErrorTree = if selected then highlight renderedExpr else renderedExpr where
    selected = maybeSelectionPath == Just []
    highlight = withAttr selectionAttr
    renderedExpr = case expr of
        IntExpr n -> str $ show n
        FnExpr var body -> str ('λ' : var) <=> (str "  " <+> renderExpr body (getChildSelectionPath 0) maybeErrorTree) where
        VarExpr var -> str var
        Call callee arg -> renderedCallee <=> (callSymbol <+> renderedArg) where
            renderedCallee = renderExpr callee (getChildSelectionPath 0) maybeCalleeError
            callSymbol = if canCallCallee then str "└ " else withAttr errorAttr (str "└ ")
            renderedArg = renderExpr arg (getChildSelectionPath 1) maybeArgError
    getChildSelectionPath index = case maybeSelectionPath of
        Just (i:childSelectionPath) -> if i == index then Just childSelectionPath else Nothing
        _ -> Nothing
    (canCallCallee, maybeCalleeError, maybeArgError) = case maybeErrorTree of
        Nothing -> (True, Nothing, Nothing)
        Just CalleeNotFn -> (False, Nothing, Nothing)
        Just (CalleeError calleeError) -> (True, Just calleeError, Nothing)
        Just (ArgError argError) -> (True, Nothing, Just argError)
        Just (CalleeNotFnAndArgError argError) -> (False, Nothing, Just argError)
        Just (CalleeAndArgError calleeError argError) -> (True, Just calleeError, Just argError)

getSubExprAtPath :: Expr -> Path -> Maybe Expr
getSubExprAtPath expr = foldl f (Just expr) where
    f maybeExpr edge = do
        expr <- maybeExpr
        case (expr, edge) of
            (FnExpr _ body, 0) -> Just body
            (Call callee _, 0) -> Just callee
            (Call _ arg, 1) -> Just arg
            _ -> Nothing

namedExprs :: Map.Map Var Expr
namedExprs = Map.fromList
    [ ("const", FnExpr "x" . FnExpr "y" $ VarExpr "x")
    , ("main", Call (Call (Call (VarExpr "const") (IntExpr 1)) (IntExpr 2)) (IntExpr 3)) ]

namedExprTypes :: Map.Map Var (Either ErrorTree Type)
namedExprTypes = inferTypes namedExprs

namedExprVals :: Map.Map Var (Maybe Val)
namedExprVals = evalExprs namedExprs

handleEvent :: State -> BrickEvent n e -> EventM n (Next State)
handleEvent state event = case event of
    VtyEvent (EvKey KUp []) -> nav prev
    VtyEvent (EvKey KDown []) -> nav next
    VtyEvent (EvKey KLeft []) -> nav parent
    VtyEvent (EvKey KRight []) -> nav child
    VtyEvent (EvKey KEnter []) -> continue goToDefinition
    VtyEvent (EvKey (KChar 'q') []) -> halt state
    _ -> continue state
    where
        State exprName selectionPath = state
        expr = fromJust $ Map.lookup exprName namedExprs
        nav = continue . State exprName
        prev = if isJust prevExpr then prevPath else selectionPath
        next = if isJust nextExpr then nextPath else selectionPath
        parent = if null selectionPath then [] else init selectionPath
        child = if isJust firstChildOfSelected then pathToFirstChildOfSelected else selectionPath
        nextExpr = getExprAtPath nextPath
        nextPath = if null selectionPath then [] else init selectionPath ++ [last selectionPath + 1]
        prevExpr = getExprAtPath prevPath
        prevPath = if null selectionPath then [] else init selectionPath ++ [last selectionPath - 1]
        selectedExpr = getExprAtPath selectionPath
        firstChildOfSelected = getExprAtPath pathToFirstChildOfSelected
        pathToFirstChildOfSelected = selectionPath ++ [0]
        getExprAtPath = getSubExprAtPath expr
        goToDefinition = case selectedExpr of
            Just (VarExpr name) -> if Map.member name namedExprs then State name [] else state
            _ -> state

attributeMap :: AttrMap
attributeMap = attrMap defAttr [ (errorAttr, fg red), (selectionAttr, withStyle defAttr bold) ]

errorAttr :: AttrName
errorAttr = attrName "error"

selectionAttr :: AttrName
selectionAttr = attrName "selection"
