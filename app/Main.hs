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
    , appAttrMap = const $ attrMap defAttr [] }

draw :: s -> [Widget n]
draw _ = [ padBottom Max (renderExpr expr) <=> str (show $ inferType expr) ]

renderExpr :: Expr -> Widget n
renderExpr = renderExprIndented ""

renderExprIndented :: String -> Expr -> Widget n
renderExprIndented indent expr = case expr of
    IntExpr n -> str $ indent ++ show n
    FnExpr var body -> str (indent ++ "λ" ++ var) <=> renderExprIndented (indent ++ "  ") body
    VarExpr var -> str $ indent ++ var
    Call f arg -> renderExprIndented indent f <=> renderExprIndented (indent ++ "  ") arg

expr :: Expr
expr = Call (Call (FnExpr "x" . FnExpr "y" $ VarExpr "x") (IntExpr 1)) (IntExpr 2)

handleEvent :: s -> BrickEvent n e -> EventM n (Next s)
handleEvent state event = case event of
    VtyEvent _ -> halt state
    _ -> continue state
