module Main where

import Control.DeepSeq
import Control.Exception.Base (evaluate)
import Control.Monad.IO.Class
import Data.List
import Data.Maybe
import System.Timeout
import Brick hiding (Location)
import Brick.Widgets.Border
import Graphics.Vty
import Eval
import Infer
import PrettyPrintType
import PrettyPrintValue
import Util
import ConstructorTypes
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NonEmpty
import qualified Defs
import qualified Expr as E
import qualified Pattern as P
import qualified Primitive
import qualified Type as T
import qualified Value as V

data AppState = AppState (Map.Map E.ExprName E.Expr) RenderMode LocationHistory InferResult EvalResult
data RenderMode = Parens | NoParens | OneWordPerLine deriving Eq
type LocationHistory = NonEmpty.NonEmpty Location -- from current to least recent
type Location = (E.ExprName, E.Path)
data EvalResult = Timeout | Error | Value V.Value
data Selectable = Expr E.Expr | Pattern P.Pattern
newtype RenderChild n = RenderChild (E.ChildIndex -> Renderer n -> RenderResult n)
type Renderer n = RenderMode -> RenderChild n -> RenderResult n
type RenderResult n = (RenderResultType, Widget n)
data RenderResultType = OneWord | OneLine | MultiLine deriving Eq

main :: IO ()
main = do
    let initialLocation = ("main", [])
    let initialLocationHistory = initialLocation NonEmpty.:| []
    initialState <- createAppState Defs.defs NoParens initialLocationHistory
    defaultMain app initialState
    return ()

createAppState :: Map.Map E.ExprName E.Expr -> RenderMode -> LocationHistory -> IO AppState
createAppState defs renderMode locationHistory = do
    let (exprName, selectionPath) = NonEmpty.head locationHistory
    let expr = fromJust $ Map.lookup exprName defs
    let inferResult = inferType constructorTypes defs expr
    let selected = fromJust $ getItemAtPathInExpr selectionPath expr
    evalResult <- createEvalResult defs selected
    return $ AppState defs renderMode locationHistory inferResult evalResult

createEvalResult :: Map.Map E.ExprName E.Expr -> Selectable -> IO EvalResult
createEvalResult defs selectable = do
    let maybeSelectedExpr = case selectable of
            Expr expr -> Just expr
            _ -> Nothing
    let maybeSelectionValue = maybeSelectedExpr >>= eval defs
    timeoutResult <- timeout 10000 $ evaluate $ force maybeSelectionValue
    return $ case timeoutResult of
        Just (Just v) -> Value v
        Just Nothing -> Error
        Nothing -> Timeout

app :: App AppState e String
app = App
    { appDraw = draw
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap = const $ attrMap defAttr [] }

draw :: AppState -> [Widget n]
draw (AppState defs renderMode locationHistory inferResult evalResult) = [ layer ] where
    layer = hBorderWithLabel title <=> padBottom Max coloredExpr <=> str bottomStr
    title = str $ "  " ++ exprName ++ "  "
    coloredExpr = modifyDefAttr (flip withForeColor gray) renderedExpr -- unselected parts of the expression are gray
    gray = rgbColor 128 128 128 -- this shade seems to work well on both light and dark backgrounds
    (exprName, selectionPath) = NonEmpty.head locationHistory
    expr = fromJust $ Map.lookup exprName defs
    (_, renderedExpr) = renderWithAttrs renderMode (Just selectionPath) maybeTypeError (renderExpr expr)
    maybeTypeError = case inferResult of
        TypeError typeError -> Just typeError
        _ -> Nothing
    maybeSelectionType = getTypeAtPathInInferResult selectionPath inferResult
    bottomStr = case maybeSelectionType of
        Just t -> evalStr ++ ": " ++ prettyPrintType t
        Nothing -> "Type error"
    evalStr = case evalResult of
        Timeout -> "<eval timeout>"
        Error -> ""
        Value v -> fromMaybe "" $ prettyPrintValue v

renderWithAttrs :: RenderMode -> Maybe E.Path -> Maybe TypeError -> Renderer n -> RenderResult n
renderWithAttrs renderMode maybeSelectionPath maybeTypeError renderer = (renderResultType, highlightIfSelected $ makeRedIfHasError widget) where
    highlightIfSelected = if selected then highlight else id
    selected = maybeSelectionPath == Just []
    makeRedIfHasError = if hasError then makeRed else id
    hasError = maybe False hasErrorAtRoot maybeTypeError
    makeRed = modifyDefAttr $ flip withForeColor red
    (renderResultType, widget) = renderer renderMode $ RenderChild renderChild
    renderChild index = renderWithAttrs renderMode (getChildPath maybeSelectionPath index) (getChildTypeError maybeTypeError index)

renderExpr :: E.Expr -> Renderer n
renderExpr expr renderMode (RenderChild renderChild) = case expr of
    E.Hole -> (OneWord, str "_")
    E.Ref exprName -> (OneWord, str exprName)
    E.Var var -> (OneWord, str var)
    E.Fn alternatives -> if null restOfAltResults then singleAltResult else multiAltResult where
        singleAltResult = (firstAltResultType, str "λ " <+> firstAltWidget)
        multiAltResult = (MultiLine, vBox $ str "λ " <+> firstAltWidget : map (str "| " <+>) restOfAltWidgets)
        (firstAltResultType, firstAltWidget) NonEmpty.:| restOfAltResults =
            NonEmpty.zipWith (renderAlternative $ RenderChild renderChild) (NonEmpty.fromList [0..]) alternatives
        restOfAltWidgets = map snd restOfAltResults
    E.Call callee arg -> if shouldBeMultiLine then multiLineResult else oneLineResult where
        shouldBeMultiLine = case renderMode of
            Parens -> calleeResultType == MultiLine || argResultType == MultiLine
            NoParens -> calleeResultType == MultiLine || argResultType /= OneWord
            OneWordPerLine -> True
        multiLineResult = (MultiLine, renderedCallee <=> indent renderedArg)
        oneLineResult = (OneLine, renderedCallee <+> str " " <+> withParensIf (argResultType /= OneWord) renderedArg)
        (calleeResultType, renderedCallee) = renderChild 0 (renderExpr callee)
        (argResultType, renderedArg) = renderChild 1 (renderExpr arg)
    E.Constructor name -> (OneWord, str name)
    E.Int n -> (OneWord, str $ show n)
    E.Primitive p -> (OneWord, str $ Primitive.getDisplayName p)

withParensIf :: Bool -> Widget n -> Widget n
withParensIf cond w = if cond then str "(" <+> w <+> str ")" else w

renderAlternative :: RenderChild n -> Int -> E.Alternative -> RenderResult n
renderAlternative (RenderChild renderChild) alternativeIndex (pattern, expr) =
    if exprResultType == MultiLine
    then (MultiLine, renderedPattern <+> str " ->" <=> renderedExpr)
    else (OneLine, renderedPattern <+> str " -> " <+> renderedExpr)
    where
        (_, renderedPattern) = renderChild (2 * alternativeIndex) $ renderPattern pattern
        (exprResultType, renderedExpr) = renderChild (2 * alternativeIndex + 1) $ renderExpr expr

renderPattern :: P.Pattern -> Renderer n
renderPattern pattern renderMode (RenderChild renderChild) = case pattern of
    P.Wildcard -> (OneWord, str "_")
    P.Var var -> (OneWord, str var)
    P.Constructor name patterns -> (OneLine, hBox $ intersperse (str " ") (str name : renderedChildren)) where
        renderedChildren = fmap snd $ zipWith renderChild [0..] renderers
        renderers = renderPattern <$> patterns

indent :: Widget n -> Widget n
indent w = str "  " <+> w

highlight :: Widget n -> Widget n
highlight = modifyDefAttr $ const defAttr -- the gray foreground color is changed back to the default

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
    Pattern pattern -> getChildInPattern pattern

getChildInExpr :: E.Expr -> E.ChildIndex -> Maybe Selectable
getChildInExpr expr index = case (expr, index) of
    (E.Fn alternatives, _) -> do
        let altIndex = div index 2
        (pattern, expr) <- getItemAtIndex altIndex (NonEmpty.toList alternatives)
        return $ if even index then Pattern pattern else Expr expr
    (E.Call callee _, 0) -> Just $ Expr callee
    (E.Call _ arg, 1) -> Just $ Expr arg
    _ -> Nothing

getChildInPattern :: P.Pattern -> E.ChildIndex -> Maybe Selectable
getChildInPattern pattern index = case pattern of
    P.Wildcard -> Nothing
    P.Var _ -> Nothing
    P.Constructor name patterns -> Pattern <$> getItemAtIndex index patterns

handleEvent :: AppState -> BrickEvent n e -> EventM n (Next AppState)
handleEvent appState event = case event of
    VtyEvent (EvKey KUp []) -> nav parentPath
    VtyEvent (EvKey KDown []) -> nav pathToFirstChildOfSelected
    VtyEvent (EvKey KLeft []) -> nav prevSiblingPath
    VtyEvent (EvKey KRight []) -> nav nextSiblingPath
    VtyEvent (EvKey KEnter []) -> goToDefinition
    VtyEvent (EvKey KEsc []) -> goBack
    VtyEvent (EvKey (KChar 'd') []) -> deleteSelected
    VtyEvent (EvKey (KChar 'r') []) -> switchToNextRenderMode
    VtyEvent (EvKey (KChar 'R') []) -> switchToPrevRenderMode
    VtyEvent (EvKey (KChar 'q') []) -> halt appState
    _ -> continue appState
    where
        AppState defs renderMode locationHistory inferResult maybeEvalResult = appState
        (exprName, selectionPath) NonEmpty.:| past = locationHistory
        expr = fromJust $ Map.lookup exprName defs
        nav path = case getExprAtPath path of
            Just exprAtPath -> liftIO getNewAppState >>= continue where
                getNewAppState = AppState defs renderMode newLocationHistory inferResult <$> getNewEvalResult
                newLocationHistory = (exprName, path) NonEmpty.:| past
                getNewEvalResult = createEvalResult defs exprAtPath
            Nothing -> continue appState
        parentPath = if null selectionPath then [] else init selectionPath
        pathToFirstChildOfSelected = selectionPath ++ [0]
        prevSiblingPath = if null selectionPath then [] else init selectionPath ++ [last selectionPath - 1]
        nextSiblingPath = if null selectionPath then [] else init selectionPath ++ [last selectionPath + 1]
        selectedExpr = getExprAtPath selectionPath
        getExprAtPath path = getItemAtPathInExpr path expr
        goToDefinition = case selectedExpr of
            Just (Expr (E.Ref exprName)) ->
                if Map.member exprName defs
                then liftIO (createAppState defs renderMode $ NonEmpty.cons (exprName, []) locationHistory) >>= continue
                else continue appState
            _ -> continue appState
        goBack = liftIO (createAppState defs renderMode $ fromMaybe locationHistory $ NonEmpty.nonEmpty past) >>= continue
        deleteSelected = modifyDef $ deleteAtPathInExpr selectionPath expr
        modifyDef newExpr = liftIO (createAppState (Map.insert exprName newExpr defs) renderMode locationHistory) >>= continue
        switchToNextRenderMode = switchRenderMode nextRenderMode
        switchToPrevRenderMode = switchRenderMode prevRenderMode
        switchRenderMode newRenderMode = continue $ AppState defs newRenderMode locationHistory inferResult maybeEvalResult
        nextRenderMode = renderModes !! mod (renderModeIndex + 1) (length renderModes)
        prevRenderMode = renderModes !! mod (renderModeIndex - 1) (length renderModes)
        renderModeIndex = fromJust $ elemIndex renderMode renderModes
        renderModes = [Parens, NoParens, OneWordPerLine]

deleteAtPathInExpr :: E.Path -> E.Expr -> E.Expr
deleteAtPathInExpr path expr = case path of
    [] -> E.Hole
    edge:restOfPath -> case expr of
        E.Fn alts -> E.Fn $ modifyItemAtIndexInNonEmpty (div edge 2) modifyAlt alts where
            modifyAlt (pattern, expr) =
                if even edge
                then (deleteAtPathInPattern restOfPath pattern, expr)
                else (pattern, deleteAtPathInExpr restOfPath expr)
        E.Call callee arg ->
            if edge == 0
            then E.Call (deleteAtPathInExpr restOfPath callee) arg
            else E.Call callee (deleteAtPathInExpr restOfPath arg)
        _ -> error "invalid path"

deleteAtPathInPattern :: E.Path -> P.Pattern -> P.Pattern
deleteAtPathInPattern path pattern = case path of
    [] -> P.Wildcard
    edge:restOfPath -> case pattern of
        P.Constructor name children -> P.Constructor name $ modifyItemAtIndex edge (deleteAtPathInPattern restOfPath) children
        _ -> error "invalid path"
