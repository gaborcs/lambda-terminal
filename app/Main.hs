module Main where

import Brick
import Graphics.Vty
import Calculus

main :: IO ()
main = defaultMain app ()

app :: App s e String
app = App
    { appDraw = draw
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap = const $ attrMap defAttr [ (errorAttr, fg red) ] }

draw :: s -> [Widget n]
draw _ = [ padBottom Max renderedExpr <=> str typeStr ] where
    renderedExpr = renderExpr expr maybeErrorTree
    (maybeErrorTree, typeStr) = case inferType expr of
        Left errorTree -> (Just errorTree, "Type error")
        Right t -> (Nothing, show t)

renderExpr :: Expr -> Maybe ErrorTree -> Widget n
renderExpr expr maybeErrorTree = case expr of
    IntExpr n -> str $ show n
    FnExpr var body -> str ('λ' : var) <=> (str "  " <+> renderExpr body maybeErrorTree)
    VarExpr var -> str var
    Call callee arg -> renderCall callee arg maybeErrorTree

renderCall :: Expr -> Expr -> Maybe ErrorTree -> Widget n
renderCall callee arg maybeErrorTree = renderedCallee <=> (callSymbol <+> renderedArg) where
    renderedCallee = renderExpr callee maybeCalleeError
    callSymbol = if canCallCallee then str "└ " else withAttr errorAttr (str "└ ")
    renderedArg = renderExpr arg maybeArgError
    (canCallCallee, maybeCalleeError, maybeArgError) = case maybeErrorTree of
        Nothing -> (True, Nothing, Nothing)
        Just CalleeNotFn -> (False, Nothing, Nothing)
        Just (CalleeError calleeError) -> (True, Just calleeError, Nothing)
        Just (ArgError argError) -> (True, Nothing, Just argError)
        Just (CalleeNotFnAndArgError argError) -> (False, Nothing, Just argError)
        Just (CalleeAndArgError calleeError argError) -> (True, Just calleeError, Just argError)

expr :: Expr
expr = Call (Call (Call (FnExpr "x" . FnExpr "y" $ VarExpr "x") (IntExpr 1)) (IntExpr 2)) (IntExpr 3)

handleEvent :: s -> BrickEvent n e -> EventM n (Next s)
handleEvent state event = case event of
    VtyEvent _ -> halt state
    _ -> continue state

errorAttr :: AttrName
errorAttr = attrName "error"
