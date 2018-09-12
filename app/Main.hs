module Main where

import Data.Maybe
import Brick
import Graphics.Vty
import Eval
import Infer
import PrettyPrintType
import PrettyPrintValue
import Util
import Defs
import ConstructorTypes
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NonEmpty
import qualified Expr as E
import qualified Pattern as P
import qualified Type as T
import qualified Value as V

data State = State E.ExprName E.Path
data Selectable = Expr E.Expr | Alternative E.Alternative | Pattern P.Pattern
newtype RenderChild n = RenderChild (E.ChildIndex -> ((Widget n -> Widget n) -> RenderChild n -> Widget n) -> Widget n)
type Renderer n = (Widget n -> Widget n) -> RenderChild n -> Widget n

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
    renderedExpr = renderWithAttrs (Just selectionPath) maybeTypeError (renderExpr expr)
    maybeTypeError = case inferResult of
        TypeError typeError -> Just typeError
        _ -> Nothing
    inferResult = inferType constructorTypes defs expr
    maybeSelectionType = getTypeAtPathInInferResult selectionPath inferResult
    bottomStr = case (maybeSelectionType, maybeSelectionValue) of
        (Just t, Just v) -> prettyPrintValue v ++ ": " ++ prettyPrintType t
        (Just t, _) -> prettyPrintType t
        _ -> "Type error"
    maybeSelectionValue = maybeSelectedExpr >>= eval defs
    maybeSelectedExpr = case selected of
        Expr expr -> Just expr
        _ -> Nothing
    selected = fromJust $ getItemAtPathInExpr selectionPath expr

renderWithAttrs :: Maybe E.Path -> Maybe TypeError -> Renderer n -> Widget n
renderWithAttrs maybeSelectionPath maybeTypeError renderer = highlightIfSelected widget where
    highlightIfSelected = if selected then highlight else id
    selected = maybeSelectionPath == Just []
    makeRedIfHasError = if hasError then makeRed else id
    hasError = maybe False hasErrorAtRoot maybeTypeError
    makeRed = modifyDefAttr $ flip withForeColor red
    widget = renderer makeRedIfHasError (RenderChild renderChild)
    renderChild index = renderWithAttrs (getChildPath maybeSelectionPath index) (getChildTypeError maybeTypeError index)

renderExpr :: E.Expr -> Renderer n
renderExpr expr makeRedIfHasError (RenderChild renderChild) = case expr of
    E.Ref exprName -> makeRedIfHasError $ str exprName
    E.Var var -> makeRedIfHasError $ str var
    E.Fn alternatives -> makeRedIfHasError $ str "λ" <+> vBox (zipWith renderChild [0..] $ renderAlternative <$> NonEmpty.toList alternatives) where
    E.Call callee arg -> renderedCallee <=> (callSymbol <+> renderedArg) where
        renderedCallee = renderChild 0 (renderExpr callee)
        callSymbol = makeRedIfHasError $ str "└ "
        renderedArg = renderChild 1 (renderExpr arg)
    E.Constructor name -> makeRedIfHasError $ str name
    E.Int n -> makeRedIfHasError . str $ show n
    E.Plus -> makeRedIfHasError $ str "+"
    E.Times -> makeRedIfHasError $ str "*"

renderAlternative :: E.Alternative -> Renderer n
renderAlternative (pattern, expr) makeRedIfHasError (RenderChild renderChild) =
    makeRedIfHasError $ renderChild 0 (renderPattern pattern) <=> (str "  " <+> renderChild 1 (renderExpr expr))

renderPattern :: P.Pattern -> Renderer n
renderPattern pattern makeRedIfHasError (RenderChild renderChild) = makeRedIfHasError $ case pattern of
    P.Var var -> str var
    P.Constructor name patterns -> str name <=> (str "  " <+> vBox (zipWith renderChild [0..] $ renderPattern <$> patterns))

highlight :: Widget n -> Widget n
highlight = modifyDefAttr $ flip withStyle bold

getChildPath :: Maybe E.Path -> E.ChildIndex -> Maybe E.Path
getChildPath maybePath index = case maybePath of
    Just (i:childPath) -> if i == index then Just childPath else Nothing
    _ -> Nothing

getChildTypeError :: Maybe TypeError -> E.ChildIndex -> Maybe TypeError
getChildTypeError maybeTypeError index = case childResult of
    Just (TypeError childError) -> Just childError
    _ -> Nothing
    where childResult = (!! index) <$> maybeTypeError

getTypeAtPathInInferResult :: E.Path -> InferResult -> Maybe T.Type
getTypeAtPathInInferResult path inferResult = case inferResult of
    Typed typeTree -> getTypeAtPathInTypeTree path typeTree
    TypeError childResults -> case path of
        [] -> Nothing
        index:restOfPath -> getTypeAtPathInInferResult restOfPath $ childResults !! index

getTypeAtPathInTypeTree :: E.Path -> TypeTree -> Maybe T.Type
getTypeAtPathInTypeTree path (TypeTree t children) = case path of
    [] -> Just t
    index:restOfPath -> getTypeAtPathInTypeTree restOfPath $ children !! index

getItemAtPathInExpr :: E.Path -> E.Expr -> Maybe Selectable
getItemAtPathInExpr path expr = getItemAtPathInSelectable path (Expr expr)

getItemAtPathInSelectable :: E.Path -> Selectable -> Maybe Selectable
getItemAtPathInSelectable path selectable = case path of
    [] -> Just selectable
    edge:restOfPath -> getChildInSelectable selectable edge >>= getItemAtPathInSelectable restOfPath

getChildInSelectable :: Selectable -> E.ChildIndex -> Maybe Selectable
getChildInSelectable selectable = case selectable of
    Expr expr -> getChildInExpr expr
    Alternative alt -> getChildInAlternative alt
    Pattern pattern -> getChildInPattern pattern

getChildInExpr :: E.Expr -> E.ChildIndex -> Maybe Selectable
getChildInExpr expr index = case (expr, index) of
    (E.Fn alternatives, _) -> Alternative <$> getItemAtIndex (NonEmpty.toList alternatives) index
    (E.Call callee _, 0) -> Just $ Expr callee
    (E.Call _ arg, 1) -> Just $ Expr arg
    _ -> Nothing

getChildInAlternative :: E.Alternative -> E.ChildIndex -> Maybe Selectable
getChildInAlternative (pattern, expr) index = case index of
    0 -> Just $ Pattern pattern
    1 -> Just $ Expr expr
    _ -> Nothing

getChildInPattern :: P.Pattern -> E.ChildIndex -> Maybe Selectable
getChildInPattern pattern index = case pattern of
    P.Var _ -> Nothing
    P.Constructor name patterns -> Pattern <$> getItemAtIndex patterns index

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
        getExprAtPath path = getItemAtPathInExpr path expr
        goToDefinition = case selectedExpr of
            Just (Expr (E.Ref exprName)) -> if Map.member exprName defs then State exprName [] else state
            _ -> state
