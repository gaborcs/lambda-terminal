module Main where

import Control.DeepSeq
import Control.Exception.Base (evaluate)
import Control.Monad.IO.Class
import Data.List
import Data.Maybe
import System.Timeout
import Text.Read
import Brick hiding (Location)
import Brick.Widgets.Border
import Brick.Widgets.Edit
import Eval
import Infer
import PrettyPrintType
import PrettyPrintValue
import Util
import ConstructorTypes
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NonEmpty
import qualified Brick.Types
import qualified Graphics.Vty as Vty
import qualified Defs
import qualified Expr as E
import qualified Pattern as P
import qualified Primitive
import qualified Type as T
import qualified Value as V

data AppState = AppState Defs RenderMode LocationHistory EditState InferResult EvalResult
data AppResourceName = EditorName deriving (Eq, Ord, Show)
type AppWidget = Widget AppResourceName
type Defs = Map.Map E.ExprName E.Expr
data RenderMode = Parens | NoParens | OneWordPerLine deriving Eq
type LocationHistory = NonEmpty.NonEmpty Location -- from current to least recent
type Location = (E.ExprName, E.Path)
type EditState = Maybe (Editor String AppResourceName)
data EvalResult = Timeout | Error | Value V.Value
data Selectable = Expr E.Expr | Pattern P.Pattern
newtype RenderChild = RenderChild (E.ChildIndex -> Renderer -> RenderResult)
type Renderer = RenderMode -> RenderChild -> RenderResult
type RenderResult = (RenderResultType, AppWidget)
data RenderResultType = OneWord | OneLine | MultiLine deriving Eq
data Selection = ContainsSelection E.Path | WithinSelection | NoSelection deriving Eq

main :: IO ()
main = do
    let initialLocation = ("main", [])
    let initialLocationHistory = initialLocation NonEmpty.:| []
    initialState <- createAppState Defs.defs NoParens initialLocationHistory
    defaultMain app initialState
    return ()

createAppState :: Defs -> RenderMode -> LocationHistory -> IO AppState
createAppState defs renderMode locationHistory = do
    let (exprName, selectionPath) = NonEmpty.head locationHistory
    let expr = fromJust $ Map.lookup exprName defs
    let inferResult = inferType constructorTypes defs expr
    let selected = fromJust $ getItemAtPathInExpr selectionPath expr
    evalResult <- createEvalResult defs selected
    return $ AppState defs renderMode locationHistory Nothing inferResult evalResult

createEvalResult :: Defs -> Selectable -> IO EvalResult
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

app :: App AppState e AppResourceName
app = App
    { appDraw = draw
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap = const $ attrMap Vty.defAttr [] }

draw :: AppState -> [AppWidget]
draw (AppState defs renderMode locationHistory editState inferResult evalResult) = [ layer ] where
    layer = hBorderWithLabel title <=> padBottom Max coloredExpr <=> str bottomStr
    title = str $ "  " ++ exprName ++ "  "
    coloredExpr = modifyDefAttr (flip Vty.withForeColor gray) renderedExpr -- unselected parts of the expression are gray
    gray = Vty.rgbColor 128 128 128 -- this shade seems to work well on both light and dark backgrounds
    (exprName, selectionPath) = NonEmpty.head locationHistory
    expr = fromJust $ Map.lookup exprName defs
    (_, renderedExpr) = renderWithAttrs renderMode editState (ContainsSelection selectionPath) maybeTypeError (renderExpr expr)
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

renderWithAttrs :: RenderMode -> EditState -> Selection -> Maybe TypeError -> Renderer -> RenderResult
renderWithAttrs renderMode editState selection maybeTypeError renderer = case editState of
    Just editor | selected -> (OneWord, highlight $ hLimit (length editStr + 1) renderedEditor) where
        editStr = head $ getEditContents editor
        renderedEditor = renderEditor (str . head) True editor
    _ -> (renderResultType, highlightIfSelected $ makeRedIfNeeded widget)
    where
        selected = selection == ContainsSelection []
        withinSelection = selection == WithinSelection
        highlightIfSelected = if selected then highlight else id
        makeRedIfNeeded = if hasError && (selected || withinSelection) then makeRed else id
        hasError = maybe False hasErrorAtRoot maybeTypeError
        makeRed = modifyDefAttr $ flip Vty.withForeColor Vty.red
        (renderResultType, widget) = renderer renderMode $ RenderChild renderChild
        renderChild index = renderWithAttrs renderMode editState (getChildSelection selection index) (getChildTypeError maybeTypeError index)

renderExpr :: E.Expr -> Renderer
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

renderAlternative :: RenderChild -> Int -> E.Alternative -> RenderResult
renderAlternative (RenderChild renderChild) alternativeIndex (pattern, expr) =
    if exprResultType == MultiLine
    then (MultiLine, renderedPattern <+> str " ->" <=> renderedExpr)
    else (OneLine, renderedPattern <+> str " -> " <+> renderedExpr)
    where
        (_, renderedPattern) = renderChild (2 * alternativeIndex) $ renderPattern pattern
        (exprResultType, renderedExpr) = renderChild (2 * alternativeIndex + 1) $ renderExpr expr

renderPattern :: P.Pattern -> Renderer
renderPattern pattern renderMode (RenderChild renderChild) = case pattern of
    P.Wildcard -> (OneWord, str "_")
    P.Var var -> (OneWord, str var)
    P.Constructor name patterns -> (OneLine, hBox $ intersperse (str " ") (str name : renderedChildren)) where
        renderedChildren = addParensIfNeeded <$> zipWith renderChild [0..] renderers
        addParensIfNeeded (resultType, renderedChild) = withParensIf (resultType /= OneWord) renderedChild
        renderers = renderPattern <$> patterns
    P.Int n -> (OneWord, str $ show n)

indent :: Widget n -> Widget n
indent w = str "  " <+> w

highlight :: Widget n -> Widget n
highlight = modifyDefAttr $ const Vty.defAttr -- the gray foreground color is changed back to the default

getChildSelection :: Selection -> E.ChildIndex -> Selection
getChildSelection selection index = case selection of
    ContainsSelection (i:childPath) -> if i == index then ContainsSelection childPath else NoSelection
    ContainsSelection [] -> WithinSelection
    WithinSelection -> WithinSelection
    NoSelection -> NoSelection

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
    P.Constructor name patterns -> Pattern <$> getItemAtIndex index patterns
    _ -> Nothing

handleEvent :: AppState -> BrickEvent n e -> EventM AppResourceName (Next AppState)
handleEvent appState (VtyEvent event) = case editState of
    Just editor -> case event of
        Vty.EvKey Vty.KEsc [] -> cancelEdit
        Vty.EvKey Vty.KEnter [] -> finishEdit $ head $ getEditContents editor
        _ -> handleEditorEvent event editor >>= setEditor
    Nothing -> case event of
        Vty.EvKey Vty.KEnter [] -> goToDefinition
        Vty.EvKey Vty.KEsc [] -> goBack
        Vty.EvKey Vty.KUp [] -> navToParent
        Vty.EvKey Vty.KDown [] -> navToChild
        Vty.EvKey Vty.KLeft [] -> navBackward
        Vty.EvKey Vty.KRight [] -> navForward
        Vty.EvKey (Vty.KChar 'p') [] -> navToParent
        Vty.EvKey (Vty.KChar 'c') [] -> navToChild
        Vty.EvKey (Vty.KChar 'b') [] -> navBackward
        Vty.EvKey (Vty.KChar 'f') [] -> navForward
        Vty.EvKey (Vty.KChar 'e') [] -> edit
        Vty.EvKey (Vty.KChar 'd') [] -> deleteSelected
        Vty.EvKey (Vty.KChar 'r') [] -> switchToNextRenderMode
        Vty.EvKey (Vty.KChar 'R') [] -> switchToPrevRenderMode
        Vty.EvKey (Vty.KChar 'q') [] -> halt appState
        _ -> continue appState
    where
        AppState defs renderMode locationHistory editState inferResult maybeEvalResult = appState
        (exprName, selectionPath) NonEmpty.:| past = locationHistory
        expr = fromJust $ Map.lookup exprName defs
        navToParent = nav parentPath
        navToChild = nav pathToFirstChildOfSelected
        navBackward = nav prevSiblingPath
        navForward = nav nextSiblingPath
        nav path = case getItemAtPath path of
            Just itemAtPath -> liftIO getNewAppState >>= continue where
                getNewAppState = AppState defs renderMode newLocationHistory editState inferResult <$> getNewEvalResult
                newLocationHistory = (exprName, path) NonEmpty.:| past
                getNewEvalResult = createEvalResult defs itemAtPath
            Nothing -> continue appState
        parentPath = if null selectionPath then [] else init selectionPath
        pathToFirstChildOfSelected = selectionPath ++ [0]
        prevSiblingPath = if null selectionPath then [] else init selectionPath ++ [last selectionPath - 1]
        nextSiblingPath = if null selectionPath then [] else init selectionPath ++ [last selectionPath + 1]
        selected = fromJust $ getItemAtPath selectionPath
        getItemAtPath path = getItemAtPathInExpr path expr
        goToDefinition = case selected of
            Expr (E.Ref exprName) ->
                if Map.member exprName defs
                then liftIO (createAppState defs renderMode $ NonEmpty.cons (exprName, []) locationHistory) >>= continue
                else continue appState
            _ -> continue appState
        goBack = liftIO (createAppState defs renderMode $ fromMaybe locationHistory $ NonEmpty.nonEmpty past) >>= continue
        edit = setEditor $ editor EditorName (Just 1) ""
        cancelEdit = setEditState Nothing
        finishEdit str = modifyDef $ case Map.lookup str constructorTypes of
            Just constructorType -> replaceSelected (E.Constructor str) (P.Constructor str wildcards) where
                wildcards = replicate constructorArity P.Wildcard
                constructorArity = arity constructorType
            Nothing -> case readMaybe str of
                Just int -> replaceSelected (E.Int int) (P.Int int)
                Nothing -> replaceSelected (E.Var str) (P.Var str)
        replaceSelected replacementIfExpr replacementIfPattern =
            replaceAtPathInExpr selectionPath replacementIfExpr replacementIfPattern expr
        setEditor newEditor = setEditState $ Just newEditor
        setEditState newEditState = continue $ AppState defs renderMode locationHistory newEditState inferResult maybeEvalResult
        deleteSelected = modifyDef $ replaceSelected (E.Hole) (P.Wildcard)
        modifyDef newExpr = liftIO (createAppState (Map.insert exprName newExpr defs) renderMode locationHistory) >>= continue
        switchToNextRenderMode = switchRenderMode nextRenderMode
        switchToPrevRenderMode = switchRenderMode prevRenderMode
        switchRenderMode newRenderMode = continue $ AppState defs newRenderMode locationHistory editState inferResult maybeEvalResult
        nextRenderMode = renderModes !! mod (renderModeIndex + 1) (length renderModes)
        prevRenderMode = renderModes !! mod (renderModeIndex - 1) (length renderModes)
        renderModeIndex = fromJust $ elemIndex renderMode renderModes
        renderModes = [Parens, NoParens, OneWordPerLine]

arity :: T.Type -> Int
arity (T.Fn _ resultType) = 1 + arity resultType
arity _ = 0

replaceAtPathInExpr :: E.Path -> E.Expr -> P.Pattern -> E.Expr -> E.Expr
replaceAtPathInExpr path replacementIfExpr replacementIfPattern expr = case path of
    [] -> replacementIfExpr
    edge:restOfPath -> case expr of
        E.Fn alts -> E.Fn $ modifyItemAtIndexInNonEmpty (div edge 2) modifyAlt alts where
            modifyAlt (pattern, expr) =
                if even edge
                then (replaceAtPathInPattern restOfPath replacementIfPattern pattern, expr)
                else (pattern, replaceAtPathInExpr restOfPath replacementIfExpr replacementIfPattern expr)
        E.Call callee arg ->
            if edge == 0
            then E.Call (replaceAtPathInExpr restOfPath replacementIfExpr replacementIfPattern callee) arg
            else E.Call callee (replaceAtPathInExpr restOfPath replacementIfExpr replacementIfPattern arg)
        _ -> error "invalid path"

replaceAtPathInPattern :: E.Path -> P.Pattern -> P.Pattern -> P.Pattern
replaceAtPathInPattern path replacement pattern = case path of
    [] -> replacement
    edge:restOfPath -> case pattern of
        P.Constructor name children -> P.Constructor name $ modifyItemAtIndex edge (replaceAtPathInPattern restOfPath replacement) children
        _ -> error "invalid path"
