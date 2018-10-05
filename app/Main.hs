module Main where

import Control.DeepSeq
import Control.Exception.Base (evaluate)
import Control.Monad.IO.Class
import Data.Char
import Data.List
import Data.Maybe
import Data.Text.Zipper
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
import qualified Data.Vector as Vec
import qualified Brick.Types
import qualified Brick.Widgets.List as ListWidget
import qualified Graphics.Vty as Vty
import qualified Defs
import qualified Expr as E
import qualified Pattern as P
import qualified Primitive
import qualified Type as T
import qualified Value as V

data AppState = AppState
    { defs :: Defs
    , wrappingStyle :: WrappingStyle
    , clipboard :: Clipboard
    , locationHistory :: LocationHistory
    , editState :: Maybe EditState
    , inferResult :: InferResult
    , evalResult :: EvalResult
    }
data AppResourceName = EditorName | AutocompleteName | Viewport deriving (Eq, Ord, Show)
type AppWidget = Widget AppResourceName
type Defs = Map.Map E.ExprName E.Expr
data WrappingStyle = NoParens | OneWordPerLine | Parens deriving (Eq, Enum, Bounded)
type LocationHistory = NonEmpty.NonEmpty Location -- from current to least recent
type Location = (E.ExprName, E.Path)
data EditState = EditState (Editor String AppResourceName) (Maybe AutocompleteState)
data AutocompleteState = AutocompleteState AutocompleteList EditorExtent
type AutocompleteList = ListWidget.List AppResourceName Selectable
type EditorExtent = Extent AppResourceName
data Clipboard = Clipboard (Maybe E.Expr) (Maybe P.Pattern)
data EvalResult = Timeout | Error | Value V.Value
data Selectable = Expr E.Expr | Pattern P.Pattern
newtype RenderChild = RenderChild (E.ChildIndex -> Renderer -> RenderResult)
type Renderer = WrappingStyle -> RenderChild -> RenderResult
type RenderResult = (RenderResultType, AppWidget)
data RenderResultType = OneWord | OneLine | MultiLine deriving Eq
data Selection = ContainsSelection E.Path | WithinSelection | NoSelection deriving Eq

main :: IO ()
main = do
    let initialClipboard = Clipboard Nothing Nothing
    let initialLocation = ("main", [])
    let initialLocationHistory = initialLocation NonEmpty.:| []
    initialState <- createAppState Defs.defs NoParens initialClipboard initialLocationHistory
    defaultMain app initialState
    return ()

createAppState :: Defs -> WrappingStyle -> Clipboard -> LocationHistory -> IO AppState
createAppState defs wrappingStyle clipboard locationHistory = do
    let (exprName, selectionPath) = NonEmpty.head locationHistory
    let expr = fromJust $ Map.lookup exprName defs
    let inferResult = inferType constructorTypes defs expr
    let selected = fromJust $ getItemAtPathInExpr selectionPath expr
    evalResult <- createEvalResult defs selected
    return $ AppState defs wrappingStyle clipboard locationHistory Nothing inferResult evalResult

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
draw (AppState defs wrappingStyle _ locationHistory maybeEditState inferResult evalResult) = ui where
    ui = case maybeEditState of
        Just (EditState _ (Just (AutocompleteState autocompleteList editorExtent))) | autocompleteListLength > 0 ->
            [ translateBy autocompleteOffset autocomplete, layout ] where
            autocompleteOffset = Brick.Types.Location (editorX, editorY + 1)
            Brick.Types.Location (editorX, editorY) = extentUpperLeft editorExtent
            autocomplete = hLimit 20 $ vLimit (min autocompleteListLength 5) $
                ListWidget.renderList renderAutocompleteItem True autocompleteList
            autocompleteListLength = length $ ListWidget.listElements autocompleteList
        _ -> [ layout ]
    layout = hBorderWithLabel title <=> viewport Viewport Both coloredExpr <=> str bottomStr
    title = str $ "  " ++ exprName ++ "  "
    coloredExpr = modifyDefAttr (flip Vty.withForeColor gray) renderedExpr -- unselected parts of the expression are gray
    gray = Vty.rgbColor 128 128 128 -- this shade seems to work well on both light and dark backgrounds
    (exprName, selectionPath) = NonEmpty.head locationHistory
    expr = fromJust $ Map.lookup exprName defs
    (_, renderedExpr) = renderWithAttrs wrappingStyle maybeEditState (ContainsSelection selectionPath) maybeTypeError (renderExpr expr)
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

renderAutocompleteItem :: Bool -> Selectable -> Widget n
renderAutocompleteItem isSelected item = color $ padRight Max $ str (printAutocompleteItem item) where
    color = modifyDefAttr $
        if isSelected
        then flip Vty.withBackColor Vty.brightCyan . flip Vty.withForeColor black
        else flip Vty.withBackColor Vty.blue . flip Vty.withForeColor white
    white = Vty.rgbColor 255 255 255
    black = Vty.rgbColor 0 0 0

renderWithAttrs :: WrappingStyle -> Maybe EditState -> Selection -> Maybe TypeError -> Renderer -> RenderResult
renderWithAttrs wrappingStyle maybeEditState selection maybeTypeError renderer = case maybeEditState of
    Just (EditState editor _) | selected -> (OneWord, visible . highlight $ hLimit (length editStr + 1) renderedEditor) where
        editStr = head $ getEditContents editor
        renderedEditor = renderEditor (str . head) True editor
    _ -> (renderResultType, (if selected then visible . highlight else id) $ makeRedIfNeeded widget)
    where
        selected = selection == ContainsSelection []
        withinSelection = selection == WithinSelection
        makeRedIfNeeded = if hasError && (selected || withinSelection) then makeRed else id
        hasError = maybe False hasErrorAtRoot maybeTypeError
        makeRed = modifyDefAttr $ flip Vty.withForeColor Vty.red
        (renderResultType, widget) = renderer wrappingStyle $ RenderChild renderChild
        renderChild index = renderWithAttrs wrappingStyle maybeEditState (getChildSelection selection index) (getChildTypeError maybeTypeError index)

renderExpr :: E.Expr -> Renderer
renderExpr expr wrappingStyle (RenderChild renderChild) = case expr of
    E.Hole -> (OneWord, str "_")
    E.Ref exprName -> (OneWord, str exprName)
    E.Var var -> (OneWord, str var)
    E.Fn alternatives -> if null restOfAltResults then singleAltResult else multiAltResult where
        singleAltResult = (firstAltResultType, str "λ " <+> firstAltWidget)
        multiAltResult = (MultiLine, vBox $ str "λ " <+> firstAltWidget : map (str "| " <+>) restOfAltWidgets)
        (firstAltResultType, firstAltWidget) NonEmpty.:| restOfAltResults =
            NonEmpty.zipWith (renderAlternative $ RenderChild renderChild) (NonEmpty.fromList [0..]) alternatives
        restOfAltWidgets = map snd restOfAltResults
    E.Call callee arg -> case callee of
        E.Fn _ -> (MultiLine, renderedMatch) where
            renderedMatch =
                if argResultType == MultiLine
                then str "match" <=> indent renderedArg <=> indent renderedCallee
                else str "match " <+> renderedArg <=> indent renderedCallee
        _ -> if shouldBeMultiLine then multiLineResult else oneLineResult where
            shouldBeMultiLine = case wrappingStyle of
                Parens -> calleeResultType == MultiLine || argResultType == MultiLine
                NoParens -> calleeResultType == MultiLine || argResultType /= OneWord
                OneWordPerLine -> True
            multiLineResult = (MultiLine, renderedCallee <=> indent renderedArg)
            oneLineResult = (OneLine, renderedCallee <+> str " " <+> withParensIf (argResultType /= OneWord) renderedArg)
        where
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
renderPattern pattern wrappingStyle (RenderChild renderChild) = case pattern of
    P.Wildcard -> (OneWord, str "_")
    P.Var var -> (OneWord, str var)
    P.Constructor name children -> (resultType, hBox $ intersperse (str " ") (str name : renderedChildren)) where
        resultType = if null children then OneWord else OneLine
        renderedChildren = addParensIfNeeded <$> zipWith renderChild [0..] childRenderers
        addParensIfNeeded (resultType, renderedChild) = withParensIf (resultType /= OneWord) renderedChild
        childRenderers = renderPattern <$> children
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
handleEvent appState (VtyEvent event) = case maybeEditState of
    Just (EditState editor maybeAutocompleteState) -> case event of
        Vty.EvKey Vty.KEsc [] -> cancelEdit
        Vty.EvKey Vty.KEnter [] -> maybe (continue appState) commitAutocompleteSelection maybeAutocompleteState
        Vty.EvKey (Vty.KChar ' ') [] -> commitEditorContent editorContent
        _ -> do
            newEditor <- case event of
                -- ignore up/down as they are used to control the autocomplete
                Vty.EvKey Vty.KUp [] -> pure editor
                Vty.EvKey Vty.KDown [] -> pure editor
                _ -> handleEditorEvent event editor
            let newEditorContent = head $ getEditContents newEditor
            let editorContentChanged = newEditorContent /= editorContent
            let isSimilarToEditorContent str = isInfixOf (map toLower newEditorContent) (map toLower str)
            let isMatch = isSimilarToEditorContent . printAutocompleteItem
            newAutocompleteList <- case maybeAutocompleteState of
                Just (AutocompleteState autocompleteList _) | not editorContentChanged -> ListWidget.handleListEvent event autocompleteList
                _ -> pure $ ListWidget.list AutocompleteName items 1 where
                    items = Vec.fromList $ filter isMatch $ case selected of
                        Expr _ -> map Expr $ vars ++ primitives ++ refs ++ constructors where
                            vars = E.Var <$> getVarsAtPath selectionPath expr
                            primitives = E.Primitive <$> [minBound..]
                            refs = E.Ref <$> Map.keys defs
                            constructors = E.Constructor <$> Map.keys constructorTypes
                        Pattern _ -> map Pattern $ uncurry createConstructorPattern <$> Map.toList constructorTypes
            maybeEditorExtent <- lookupExtent EditorName
            setEditState $ Just $ EditState newEditor $ AutocompleteState newAutocompleteList <$> maybeEditorExtent
        where editorContent = head $ getEditContents editor
    Nothing -> case event of
        Vty.EvKey Vty.KEnter [] -> goToDefinition
        Vty.EvKey Vty.KEsc [] -> goBack
        Vty.EvKey Vty.KUp [] -> navToParent
        Vty.EvKey Vty.KDown [] -> navToChild
        Vty.EvKey Vty.KLeft [] -> navBackward
        Vty.EvKey Vty.KRight [] -> navForward
        Vty.EvKey (Vty.KChar 'i') [] -> navToParent
        Vty.EvKey (Vty.KChar 'k') [] -> navToChild
        Vty.EvKey (Vty.KChar 'j') [] -> navBackward
        Vty.EvKey (Vty.KChar 'l') [] -> navForward
        Vty.EvKey (Vty.KChar 'e') [] -> edit
        Vty.EvKey (Vty.KChar ' ') [] -> callSelected
        Vty.EvKey (Vty.KChar 'a') [] -> applyFnToSelected
        Vty.EvKey (Vty.KChar 'λ') [] -> wrapSelectedInFn
        Vty.EvKey (Vty.KChar '\\') [] -> wrapSelectedInFn
        Vty.EvKey (Vty.KChar '|') [] -> addAlternativeToSelected
        Vty.EvKey (Vty.KChar 'd') [] -> deleteSelected
        Vty.EvKey (Vty.KChar 'r') [] -> switchToNextWrappingStyle
        Vty.EvKey (Vty.KChar 'R') [] -> switchToPrevWrappingStyle
        Vty.EvKey (Vty.KChar 'c') [] -> copy
        Vty.EvKey (Vty.KChar 'p') [] -> paste
        Vty.EvKey (Vty.KChar 'q') [] -> halt appState
        _ -> continue appState
    where
        AppState defs wrappingStyle clipboard locationHistory maybeEditState inferResult maybeEvalResult = appState
        (exprName, selectionPath) NonEmpty.:| past = locationHistory
        expr = fromJust $ Map.lookup exprName defs
        navToParent = nav parentPath
        navToChild = nav pathToFirstChildOfSelected
        navBackward = nav prevSiblingPath
        navForward = nav nextSiblingPath
        nav path = case getItemAtPath path of
            Just itemAtPath -> liftIO getNewAppState >>= continue where
                getNewAppState = do
                    newEvalResult <- createEvalResult defs itemAtPath
                    return $ appState { locationHistory = (exprName, path) NonEmpty.:| past, evalResult = newEvalResult }
            Nothing -> continue appState
        parentPath = if null selectionPath then [] else init selectionPath
        pathToFirstChildOfSelected = selectionPath ++ [0]
        prevSiblingPath = if null selectionPath then [] else init selectionPath ++ [mod (last selectionPath - 1) siblingCount]
        nextSiblingPath = if null selectionPath then [] else init selectionPath ++ [mod (last selectionPath + 1) siblingCount]
        siblingCount = case getItemAtPath (init selectionPath) of
            Just (Expr (E.Fn alts)) -> 2 * length alts
            Just (Expr (E.Call _ _)) -> 2
            Just (Pattern (P.Constructor _ siblings)) -> length siblings
            _ -> 1
        selected = fromJust $ getItemAtPath selectionPath
        getItemAtPath path = getItemAtPathInExpr path expr
        goToDefinition = case selected of
            Expr (E.Ref exprName) ->
                if Map.member exprName defs
                then liftIO (createAppState defs wrappingStyle clipboard $ NonEmpty.cons (exprName, []) locationHistory) >>= continue
                else continue appState
            _ -> continue appState
        goBack = liftIO (createAppState defs wrappingStyle clipboard $ fromMaybe locationHistory $ NonEmpty.nonEmpty past) >>= continue
        edit = setEditState $ Just $ EditState initialEditor Nothing
        initialEditor = applyEdit gotoEOL $ editor EditorName (Just 1) initialEditorContent
        initialEditorContent = printAutocompleteItem selected
        cancelEdit = setEditState Nothing
        commitEditorContent editorContent = modifyDef $ case Map.lookup editorContent constructorTypes of
            Just constructorType -> replaceSelected (E.Constructor editorContent) (createConstructorPattern editorContent constructorType)
            Nothing -> case readMaybe editorContent of
                Just int -> replaceSelected (E.Int int) (P.Int int)
                Nothing -> replaceSelected (E.Var editorContent) (P.Var editorContent)
        commitAutocompleteSelection (AutocompleteState autocompleteList _) = case ListWidget.listSelectedElement autocompleteList of
            Just (_, selectedItem) -> modifyDef $ case selectedItem of
                Expr expr -> modifySelected (const expr) id
                Pattern pattern -> modifySelected id (const pattern)
            _ -> continue appState
        replaceSelected replacementIfExpr replacementIfPattern = modifySelected (const replacementIfExpr) (const replacementIfPattern)
        modifySelected modifyExpr modifyPattern = modifyAtPathInExpr selectionPath modifyExpr modifyPattern expr
        setEditState newEditState = continue $ appState { editState = newEditState }
        callSelected = modifySelectedExpr $ \expr -> E.Call expr E.Hole
        applyFnToSelected = modifySelectedExpr $ E.Call E.Hole
        wrapSelectedInFn = modifySelectedExpr $ \expr -> E.Fn (pure (P.Wildcard, expr))
        modifySelectedExpr modify = liftIO (createAppState newDefs wrappingStyle clipboard newLocationHistory) >>= continue where
            newDefs = Map.insert exprName newExpr defs
            newExpr = modifyAtPathInExpr pathToExprContainingSelection modify id expr
            newLocationHistory = (exprName, pathToExprContainingSelection) NonEmpty.:| past
            pathToExprContainingSelection = dropPatternPartOfPath expr selectionPath
        addAlternativeToSelected = maybe (continue appState) modifyDef (addAlternativeAtPath selectionPath expr)
        deleteSelected = modifyDef $ replaceSelected E.Hole P.Wildcard
        modifyDef newExpr = liftIO (createAppState (Map.insert exprName newExpr defs) wrappingStyle clipboard locationHistory) >>= continue
        switchToNextWrappingStyle = switchWrappingStyle $ if wrappingStyle == maxBound then minBound else succ wrappingStyle
        switchToPrevWrappingStyle = switchWrappingStyle $ if wrappingStyle == minBound then maxBound else pred wrappingStyle
        switchWrappingStyle newWrappingStyle = continue $ appState { wrappingStyle = newWrappingStyle }
        copy = continue $ appState { clipboard = clipboardAfterCopy }
        clipboardAfterCopy = case selected of
            Expr e -> Clipboard (Just e) patternClipboard
            Pattern p -> Clipboard exprClipboard (Just p)
        paste = modifyDef $ modifySelected (maybe id const exprClipboard) (maybe id const patternClipboard)
        Clipboard exprClipboard patternClipboard = clipboard

printAutocompleteItem :: Selectable -> String
printAutocompleteItem item = case item of
    Expr (E.Ref name) -> name
    Expr (E.Var name) -> name
    Expr (E.Constructor name) -> name
    Expr (E.Int n) -> show n
    Expr (E.Primitive p) -> Primitive.getDisplayName p
    Pattern (P.Var name) -> name
    Pattern (P.Constructor name _) -> name
    Pattern (P.Int n) -> show n
    _ -> ""

createConstructorPattern :: E.ConstructorName -> T.Type -> P.Pattern
createConstructorPattern name t = P.Constructor name wildcards where
    wildcards = replicate (arity t) P.Wildcard

arity :: T.Type -> Int
arity (T.Fn _ resultType) = 1 + arity resultType
arity _ = 0

getVarsAtPath :: E.Path -> E.Expr -> [E.VarName]
getVarsAtPath path expr = case path of
    [] -> []
    edge:restOfPath -> case expr of
        E.Fn alts | odd edge -> getVars pattern ++ getVarsAtPath restOfPath body where
            (pattern, body) = alts NonEmpty.!! div edge 2
        E.Call callee _ | edge == 0 -> getVarsAtPath restOfPath callee
        E.Call _ arg | edge == 1 -> getVarsAtPath restOfPath arg
        _ -> error "invalid path"

getVars :: P.Pattern -> [E.VarName]
getVars (P.Var name) = [name]
getVars (P.Constructor _ children) = children >>= getVars
getVars _ = []

modifyAtPathInExpr :: E.Path -> (E.Expr -> E.Expr) -> (P.Pattern -> P.Pattern) -> E.Expr -> E.Expr
modifyAtPathInExpr path modifyExpr modifyPattern expr = case path of
    [] -> modifyExpr expr
    edge:restOfPath -> case expr of
        E.Fn alts -> E.Fn $ modifyItemAtIndexInNonEmpty (div edge 2) modifyAlt alts where
            modifyAlt (pattern, expr) =
                if even edge
                then (modifyAtPathInPattern restOfPath modifyPattern pattern, expr)
                else (pattern, modifyAtPathInExpr restOfPath modifyExpr modifyPattern expr)
        E.Call callee arg | edge == 0 ->
            E.Call (modifyAtPathInExpr restOfPath modifyExpr modifyPattern callee) arg
        E.Call callee arg | edge == 1 ->
            E.Call callee (modifyAtPathInExpr restOfPath modifyExpr modifyPattern arg)
        _ -> error "invalid path"

modifyAtPathInPattern :: E.Path -> (P.Pattern -> P.Pattern) -> P.Pattern -> P.Pattern
modifyAtPathInPattern path modify pattern = case path of
    [] -> modify pattern
    edge:restOfPath -> case pattern of
        P.Constructor name children -> P.Constructor name $ modifyItemAtIndex edge (modifyAtPathInPattern restOfPath modify) children
        _ -> error "invalid path"

dropPatternPartOfPath :: E.Expr -> E.Path -> E.Path
dropPatternPartOfPath expr path = case path of
    [] -> []
    edge:restOfPath -> case expr of
        E.Fn alts -> if even edge then [] else edge : dropPatternPartOfPath (snd $ alts NonEmpty.!! div edge 2) restOfPath
        E.Call callee arg | edge == 0 -> 0 : dropPatternPartOfPath callee restOfPath
        E.Call callee arg | edge == 1 -> 1 : dropPatternPartOfPath arg restOfPath
        _ -> error "invalid path"

addAlternativeAtPath :: E.Path -> E.Expr -> Maybe E.Expr
addAlternativeAtPath selectionPath expr = case (expr, selectionPath) of
    (E.Fn alts, edge:restOfPath) | odd edge -> case addAlternativeAtPath restOfPath (snd $ alts NonEmpty.!! altIndex) of
        Just expr -> Just $ E.Fn $ modifyItemAtIndexInNonEmpty altIndex modifyAlt alts where
            modifyAlt (pattern, _) = (pattern, expr)
        _ -> Just $ E.Fn $ alts <> pure (P.Wildcard, E.Hole)
        where altIndex = div edge 2
    (E.Fn alts, _) -> Just $ E.Fn $ alts <> pure (P.Wildcard, E.Hole)
    (E.Call callee arg, 0:restOfPath) -> E.Call <$> addAlternativeAtPath restOfPath callee <*> pure arg
    (E.Call callee arg, 1:restOfPath) -> E.Call callee <$> addAlternativeAtPath restOfPath arg
    _ -> Nothing
