module Main where

import Data.Maybe
import Brick
import Graphics.Vty
import Eval
import Infer
import qualified Data.Map as Map
import qualified Expr as E
import qualified Value as V

data State = State E.ExprName E.Path

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
    , appAttrMap = const $ attrMap defAttr [] }

draw :: State -> [Widget n]
draw state = [ padBottom Max renderedExpr <=> str bottomStr ] where
    State exprName selectionPath = state
    expr = fromJust $ Map.lookup exprName defs
    renderedExpr = renderExpr expr (Just selectionPath) maybeErrorPath
    (maybeErrorPath, maybeSelectionType) = inferType defs expr selectionPath
    bottomStr = case (maybeSelectionType, eval defs selectedExpr) of
        (Just t, Just (V.Int v)) -> show v ++ ": " ++ show t
        (Just t, _) -> show t
        _ -> "Type error"
    selectedExpr = fromJust $ getSubExprAtPath expr selectionPath

renderExpr :: E.Expr -> Maybe E.Path -> Maybe E.Path -> Widget n
renderExpr expr maybeSelectionPath maybeErrorPath = highlightIfSelected renderedExpr where
    highlightIfSelected = if selected then highlight else id
    selected = maybeSelectionPath == Just []
    highlight = modifyDefAttr $ flip withStyle bold
    makeRedIfHasError = if hasError then makeRed else id
    hasError = maybeErrorPath == Just []
    makeRed = modifyDefAttr $ flip withForeColor red
    renderedExpr = case expr of
        E.Ref exprName -> makeRedIfHasError $ str exprName
        E.Var var -> makeRedIfHasError $ str var
        E.Fn var body -> makeRedIfHasError $ str ('λ' : var) <=> (str "  " <+> renderedBody) where
            renderedBody = renderExpr body (getChildSelectionPath 0) (getChildErrorPath 0)
        E.Call callee arg -> renderedCallee <=> (callSymbol <+> renderedArg) where
            renderedCallee = renderExpr callee (getChildSelectionPath 0) (getChildErrorPath 0)
            callSymbol = makeRedIfHasError $ str "└ "
            renderedArg = renderExpr arg (getChildSelectionPath 1) (getChildErrorPath 1)
        E.Int n -> makeRedIfHasError . str $ show n
    getChildSelectionPath = getChildPath maybeSelectionPath
    getChildErrorPath = getChildPath maybeErrorPath
    getChildPath maybePath index = case maybePath of
        Just (i:childPath) -> if i == index then Just childPath else Nothing
        _ -> Nothing

getSubExprAtPath :: E.Expr -> E.Path -> Maybe E.Expr
getSubExprAtPath expr = foldl f (Just expr) where
    f maybeExpr edge = do
        expr <- maybeExpr
        case (expr, edge) of
            (E.Fn _ body, 0) -> Just body
            (E.Call callee _, 0) -> Just callee
            (E.Call _ arg, 1) -> Just arg
            _ -> Nothing

defs :: Map.Map E.ExprName E.Expr
defs = Map.fromList
    [ ("const", E.Fn "x" . E.Fn "y" $ E.Var "x")
    , ("main", E.Call (E.Call (E.Call (E.Ref "const") (E.Int 1)) (E.Int 2)) (E.Int 3)) ]

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
        expr = fromJust $ Map.lookup exprName defs
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
            Just (E.Ref exprName) -> if Map.member exprName defs then State exprName [] else state
            _ -> state
