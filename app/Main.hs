module Main where

import Data.Maybe
import Brick
import Graphics.Vty
import Calculus

type Path = [ChildIndex]
type ChildIndex = Int

main :: IO ()
main = do
    defaultMain app []
    return ()

app :: App Path e String
app = App
    { appDraw = draw
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap = const attributeMap }

draw :: Path -> [Widget n]
draw selectionPath = [ padBottom Max renderedExpr <=> str typeStr ] where
    renderedExpr = renderExpr expr (Just selectionPath) maybeErrorTree
    maybeErrorTree = either Just (const Nothing) (inferType expr)
    typeStr = either (const "Type error") show (inferType selectedExpr)
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

expr :: Expr
expr = Call (Call (Call (FnExpr "x" . FnExpr "y" $ VarExpr "x") (IntExpr 1)) (IntExpr 2)) (IntExpr 3)

handleEvent :: Path -> BrickEvent n e -> EventM n (Next Path)
handleEvent selectionPath event = case event of
    VtyEvent (EvKey (KChar 'h') []) -> continue navUp
    VtyEvent (EvKey (KChar 'j') []) -> continue navNext
    VtyEvent (EvKey (KChar 'k') []) -> continue navPrev
    VtyEvent (EvKey (KChar 'l') []) -> continue navDown
    VtyEvent (EvKey (KChar 'q') []) -> halt selectionPath
    _ -> continue selectionPath
    where
        navUp = if null selectionPath then [] else init selectionPath
        navNext = if isJust nextExpr then nextPath else selectionPath
        navPrev = if isJust prevExpr then prevPath else selectionPath
        navDown = if isJust firstChildOfSelected then pathToFirstChildOfSelected else selectionPath
        nextExpr = getExprAtPath nextPath
        nextPath = if null selectionPath then [] else init selectionPath ++ [last selectionPath + 1]
        prevExpr = getExprAtPath prevPath
        prevPath = if null selectionPath then [] else init selectionPath ++ [last selectionPath - 1]
        selectedExpr = getExprAtPath selectionPath
        firstChildOfSelected = getExprAtPath pathToFirstChildOfSelected
        pathToFirstChildOfSelected = selectionPath ++ [0]
        getExprAtPath = getSubExprAtPath expr

attributeMap :: AttrMap
attributeMap = attrMap defAttr [ (errorAttr, fg red), (selectionAttr, withStyle defAttr bold) ]

errorAttr :: AttrName
errorAttr = attrName "error"

selectionAttr :: AttrName
selectionAttr = attrName "selection"
