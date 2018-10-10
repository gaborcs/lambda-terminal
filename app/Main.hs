{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.DeepSeq
import Control.Exception.Base (evaluate)
import Control.Lens hiding (DefName)
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
import History
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
    { _defs :: Map.Map DefId (Maybe DefName, History Expr)
    , _locationHistory :: History Location
    , _wrappingStyle :: WrappingStyle
    , _clipboard :: Clipboard
    , _editState :: EditState
    , _derivedState :: Maybe DerivedState
    }
data Clipboard = Clipboard
    { _clipboardExpr :: Maybe Expr
    , _clipboardPattern :: Maybe P.Pattern
    }
data DerivedState = DerivedState
    { _inferResult :: InferResult
    , _evalResult :: EvalResult
    }
type DefId = Int
type DefName = String
type Expr = E.Expr DefId
data AppResourceName = DefListName | EditorName | AutocompleteName | Viewport deriving (Eq, Ord, Show)
type AppWidget = Widget AppResourceName
data WrappingStyle = NoParens | OneWordPerLine | Parens deriving (Eq, Enum, Bounded)
data Location = DefListView SelectedDefId | DefView DefViewLocation
type SelectedDefId = DefId
type DefViewLocation = (DefId, E.Path)
data EditState = NotEditing | Naming EditorState | SelectionEditing EditorState (Maybe AutocompleteState)
type EditorState = Editor String AppResourceName
data AutocompleteState = AutocompleteState AutocompleteList EditorExtent
type AutocompleteList = ListWidget.List AppResourceName Selectable
type EditorExtent = Extent AppResourceName
data EvalResult = Timeout | Error | Value V.Value
data Selectable = Expr Expr | Pattern P.Pattern
newtype RenderChild = RenderChild (E.ChildIndex -> Renderer -> RenderResult)
type Renderer = WrappingStyle -> RenderChild -> RenderResult
type RenderResult = (RenderResultType, AppWidget)
data RenderResultType = OneWord | OneLine | MultiLine deriving Eq
data Selection = ContainsSelection E.Path | WithinSelection | NoSelection deriving Eq

makeLenses ''AppState
makeLenses ''Clipboard
makeLenses ''DerivedState
makePrisms ''Location
makePrisms ''Selectable

main :: IO AppState
main = defaultMain app initialState

initialState :: AppState
initialState = AppState defs locationHistory NoParens clipboard NotEditing Nothing where
    defs = (\(name, expr) -> (Just name, History.create expr)) <$> Defs.defs
    clipboard = Clipboard Nothing Nothing
    locationHistory = History.create $ DefListView 0

updateDerivedState :: AppState -> IO AppState
updateDerivedState appState = do
    let defExprs = view present . snd <$> view defs appState
    let location = view present $ view locationHistory appState
    newDerivedState <- traverse (createDerivedState defExprs) (preview _DefView location)
    return $ appState & derivedState .~ newDerivedState

updateEvalResult :: AppState -> IO AppState
updateEvalResult appState = do
    let defExprs = view present . snd <$> view defs appState
    let location = view present $ view locationHistory appState
    newEvalResult <- traverse (createEvalResult defExprs) (preview _DefView location)
    return $ appState & derivedState . _Just . evalResult %~ flip fromMaybe newEvalResult

createDerivedState :: Map.Map DefId Expr -> DefViewLocation -> IO DerivedState
createDerivedState defs location@(exprName, _) =
    DerivedState (createInferResult defs exprName) <$> createEvalResult defs location

createInferResult :: Map.Map DefId Expr -> DefId -> InferResult
createInferResult defs defId = inferType constructorTypes defs expr where
    expr = fromJust $ Map.lookup defId defs

createEvalResult :: Map.Map DefId Expr -> DefViewLocation -> IO EvalResult
createEvalResult defs (defId, selectionPath) = do
    let expr = fromJust $ Map.lookup defId defs
    let selected = fromJust $ getItemAtPathInExpr selectionPath expr
    let maybeSelectedExpr = preview _Expr selected
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
draw appState = case view present $ view locationHistory appState of
    DefListView selectedDefId -> drawDefListView appState selectedDefId
    DefView defViewLocation -> drawDefView appState defViewLocation

drawDefListView :: AppState -> SelectedDefId -> [AppWidget]
drawDefListView appState selectedDefId = [ renderTitle "Definitions" <=> renderedList ] where
    renderedList = vBox $ renderItem <$> Map.toAscList (view defs appState)
    renderItem (defId, (maybeName, _)) = grayIf (defId /= selectedDefId) (str $ fromMaybe "unnamed" maybeName)
    grayIf cond = if cond then modifyDefAttr $ flip Vty.withForeColor gray else id

gray :: Vty.Color
gray = Vty.rgbColor 128 128 128 -- this shade seems ok on both light and dark backgrounds

drawDefView :: AppState -> DefViewLocation -> [AppWidget]
drawDefView (AppState defs _ wrappingStyle _ editState (Just (DerivedState inferResult evalResult))) (defId, selectionPath) = ui where
    ui = case editState of
        SelectionEditing _ (Just (AutocompleteState autocompleteList editorExtent)) | autocompleteListLength > 0 ->
            [ translateBy autocompleteOffset autocomplete, layout ] where
            autocompleteOffset = Brick.Types.Location (editorX, editorY + 1)
            Brick.Types.Location (editorX, editorY) = extentUpperLeft editorExtent
            autocomplete = hLimit 20 $ vLimit (min autocompleteListLength 5) $
                ListWidget.renderList renderAutocompleteItem True $ printAutocompleteItem defNames <$> autocompleteList
            autocompleteListLength = length $ ListWidget.listElements autocompleteList
        _ -> [ layout ]
    layout = renderedTitle <=> viewport Viewport Both coloredExpr <=> str bottomStr
    renderedTitle = case editState of
        Naming editor -> prompt <+> renderEditor (str . head) True editor where
            prompt = str $ if isJust maybeDefName then "Rename to: " else "Name to: "
        _ -> renderTitle $ fromMaybe "unnamed" maybeDefName
    (maybeDefName, defHistory) = fromJust $ Map.lookup defId defs
    coloredExpr = modifyDefAttr (flip Vty.withForeColor gray) renderedExpr -- unselected parts of the expression are gray
    expr = view present defHistory
    (_, renderedExpr) = renderWithAttrs wrappingStyle editState (ContainsSelection selectionPath) maybeTypeError (renderExpr defNames expr)
    defNames = Map.map fromJust $ Map.filter isJust $ Map.map fst defs
    maybeTypeError = preview _TypeError inferResult
    maybeSelectionType = getTypeAtPathInInferResult selectionPath inferResult
    bottomStr = case maybeSelectionType of
        Just t -> evalStr ++ ": " ++ prettyPrintType t
        Nothing -> "Type error"
    evalStr = case evalResult of
        Timeout -> "<eval timeout>"
        Error -> ""
        Value v -> fromMaybe "" $ prettyPrintValue v

renderTitle :: String -> Widget n
renderTitle title = hBorderWithLabel $ str $ "  " ++ title ++ "  "

renderAutocompleteItem :: Bool -> String -> Widget n
renderAutocompleteItem isSelected text = color $ padRight Max $ str text where
    color = modifyDefAttr $
        if isSelected
        then flip Vty.withBackColor Vty.brightCyan . flip Vty.withForeColor black
        else flip Vty.withBackColor Vty.blue . flip Vty.withForeColor white
    white = Vty.rgbColor 255 255 255
    black = Vty.rgbColor 0 0 0

renderWithAttrs :: WrappingStyle -> EditState -> Selection -> Maybe TypeError -> Renderer -> RenderResult
renderWithAttrs wrappingStyle editState selection maybeTypeError renderer = case editState of
    SelectionEditing editor _ | selected -> (OneWord, visible . highlight $ hLimit (length editStr + 1) renderedEditor) where
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
        renderChild index = renderWithAttrs wrappingStyle editState (getChildSelection selection index) (getChildTypeError maybeTypeError index)

renderExpr :: Ord d => Map.Map d DefName -> E.Expr d -> Renderer
renderExpr defNames expr wrappingStyle (RenderChild renderChild) = case expr of
    E.Hole -> (OneWord, str "_")
    E.Def defId -> (OneWord, str $ fromMaybe "missing" $ Map.lookup defId defNames)
    E.Var var -> (OneWord, str var)
    E.Fn alternatives -> if null restOfAltResults then singleAltResult else multiAltResult where
        singleAltResult = (firstAltResultType, str "λ " <+> firstAltWidget)
        multiAltResult = (MultiLine, vBox $ str "λ " <+> firstAltWidget : map (str "| " <+>) restOfAltWidgets)
        (firstAltResultType, firstAltWidget) NonEmpty.:| restOfAltResults =
            NonEmpty.zipWith (renderAlternative defNames $ RenderChild renderChild) (NonEmpty.fromList [0..]) alternatives
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
            (calleeResultType, renderedCallee) = renderChild 0 (renderExpr defNames callee)
            (argResultType, renderedArg) = renderChild 1 (renderExpr defNames arg)
    E.Constructor name -> (OneWord, str name)
    E.Int n -> (OneWord, str $ show n)
    E.Primitive p -> (OneWord, str $ Primitive.getDisplayName p)

withParensIf :: Bool -> Widget n -> Widget n
withParensIf cond w = if cond then str "(" <+> w <+> str ")" else w

renderAlternative :: Ord d => Map.Map d DefName -> RenderChild -> Int -> E.Alternative d -> RenderResult
renderAlternative defNames (RenderChild renderChild) alternativeIndex (pattern, expr) =
    if exprResultType == MultiLine
    then (MultiLine, renderedPattern <+> str " ->" <=> renderedExpr)
    else (OneLine, renderedPattern <+> str " -> " <+> renderedExpr)
    where
        (_, renderedPattern) = renderChild (2 * alternativeIndex) $ renderPattern pattern
        (exprResultType, renderedExpr) = renderChild (2 * alternativeIndex + 1) $ renderExpr defNames expr

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
getChildTypeError maybeTypeError index = childResult >>= preview _TypeError
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

getItemAtPathInExpr :: E.Path -> Expr -> Maybe Selectable
getItemAtPathInExpr path expr = getItemAtPathInSelectable path (Expr expr)

getItemAtPathInSelectable :: E.Path -> Selectable -> Maybe Selectable
getItemAtPathInSelectable path selectable = case path of
    [] -> Just selectable
    edge:restOfPath -> getChildInSelectable selectable edge >>= getItemAtPathInSelectable restOfPath

getChildInSelectable :: Selectable -> E.ChildIndex -> Maybe Selectable
getChildInSelectable selectable = case selectable of
    Expr expr -> getChildInExpr expr
    Pattern pattern -> getChildInPattern pattern

getChildInExpr :: Expr -> E.ChildIndex -> Maybe Selectable
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
    P.Constructor _ patterns -> Pattern <$> getItemAtIndex index patterns
    _ -> Nothing

handleEvent :: AppState -> BrickEvent n e -> EventM AppResourceName (Next AppState)
handleEvent appState (VtyEvent event) = case view present $ view locationHistory appState of
    DefListView selectedId -> handleEventOnDefListView appState event selectedId
    DefView defViewLocation -> handleEventOnDefView appState event defViewLocation

handleEventOnDefListView :: AppState -> Vty.Event -> DefId -> EventM AppResourceName (Next AppState)
handleEventOnDefListView appState event selectedDefId = case event of
    Vty.EvKey Vty.KEnter [] -> goToDef selectedDefId appState
    Vty.EvKey Vty.KUp [] -> select $ fromMaybe selectedDefId maybePrevDefId
    Vty.EvKey Vty.KDown [] -> select $ fromMaybe selectedDefId maybeNextDefId
    Vty.EvKey (Vty.KChar 'g') [] -> goBackInLocationHistory appState
    Vty.EvKey (Vty.KChar 'G') [] -> goForwardInLocationHistory appState
    Vty.EvKey (Vty.KChar 'n') [] -> addNewDef
    Vty.EvKey (Vty.KChar 'q') [] -> halt appState
    _ -> continue appState
    where
        select defId = continue $ appState & locationHistory . present . _DefListView .~ defId
        maybePrevDefId = fmap (subtract 1) maybeSelectedIndex >>= flip getItemAtIndex defIds
        maybeNextDefId = fmap (+ 1) maybeSelectedIndex >>= flip getItemAtIndex defIds
        maybeSelectedIndex = elemIndex selectedDefId defIds
        defIds = Map.keys $ view defs appState
        addNewDef = liftIO createNewAppState >>= continue where
            createNewAppState = updateDerivedState $ appState
                & defs %~ Map.insert newDefId (Nothing, History.create E.Hole)
                & locationHistory %~ (push (DefView (newDefId, []))) . (present .~ DefListView newDefId)
            newDefId = last defIds + 1

handleEventOnDefView :: AppState -> Vty.Event -> DefViewLocation -> EventM AppResourceName (Next AppState)
handleEventOnDefView appState event (defId, selectionPath) = case currentEditState of
    Naming editor -> case event of
        Vty.EvKey Vty.KEsc [] -> cancelEdit
        Vty.EvKey Vty.KEnter [] -> commitName $ head $ getEditContents editor
        Vty.EvKey (Vty.KChar ' ') [] -> commitName $ head $ getEditContents editor
        _ -> do
            newEditor <- handleEditorEvent event editor
            continue $ appState & editState .~ Naming newEditor
    SelectionEditing editor maybeAutocompleteState -> case event of
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
            let isMatch = isSimilarToEditorContent . printAutocompleteItem defNames
            newAutocompleteList <- case maybeAutocompleteState of
                Just (AutocompleteState autocompleteList _) | not editorContentChanged -> ListWidget.handleListEvent event autocompleteList
                _ -> pure $ ListWidget.list AutocompleteName items 1 where
                    items = Vec.fromList $ filter isMatch $ case selected of
                        Expr _ -> map Expr $ vars ++ primitives ++ defs ++ constructors where
                            vars = E.Var <$> getVarsAtPath selectionPath defExpr
                            primitives = E.Primitive <$> [minBound..]
                            defs = E.Def <$> defIds
                            constructors = E.Constructor <$> Map.keys constructorTypes
                        Pattern _ -> map Pattern $ uncurry createConstructorPattern <$> Map.toList constructorTypes
            maybeEditorExtent <- lookupExtent EditorName
            setEditState $ SelectionEditing newEditor $ AutocompleteState newAutocompleteList <$> maybeEditorExtent
        where editorContent = head $ getEditContents editor
    NotEditing -> case event of
        Vty.EvKey Vty.KEnter [] -> goToDefinition
        Vty.EvKey (Vty.KChar 'g') [] -> goBackInLocationHistory appState
        Vty.EvKey (Vty.KChar 'G') [] -> goForwardInLocationHistory appState
        Vty.EvKey (Vty.KChar 'n') [] -> initiateRename
        Vty.EvKey Vty.KUp [] -> navToParent
        Vty.EvKey Vty.KDown [] -> navToChild
        Vty.EvKey Vty.KLeft [] -> navBackward
        Vty.EvKey Vty.KRight [] -> navForward
        Vty.EvKey (Vty.KChar 'i') [] -> navToParent
        Vty.EvKey (Vty.KChar 'k') [] -> navToChild
        Vty.EvKey (Vty.KChar 'j') [] -> navBackward
        Vty.EvKey (Vty.KChar 'l') [] -> navForward
        Vty.EvKey (Vty.KChar 'e') [] -> initiateSelectionEdit
        Vty.EvKey (Vty.KChar ' ') [] -> callSelected
        Vty.EvKey (Vty.KChar 'a') [] -> applyFnToSelected
        Vty.EvKey (Vty.KChar 'λ') [] -> wrapSelectedInFn
        Vty.EvKey (Vty.KChar '\\') [] -> wrapSelectedInFn
        Vty.EvKey (Vty.KChar '|') [] -> addAlternativeToSelected
        Vty.EvKey (Vty.KChar 'd') [] -> deleteSelected
        Vty.EvKey (Vty.KChar '\t') [] -> switchToNextWrappingStyle
        Vty.EvKey Vty.KBackTab [] -> switchToPrevWrappingStyle
        Vty.EvKey (Vty.KChar 'c') [] -> copy
        Vty.EvKey (Vty.KChar 'p') [] -> paste
        Vty.EvKey (Vty.KChar 'u') [] -> undo
        Vty.EvKey (Vty.KChar 'r') [] -> redo
        Vty.EvKey (Vty.KChar 'q') [] -> halt appState
        _ -> continue appState
    where
        currentDefs = view defs appState
        defIds = Map.keys currentDefs
        defNames = Map.map fromJust $ Map.filter isJust $ Map.map fst currentDefs
        (maybeDefName, defHistory) = fromJust $ Map.lookup defId currentDefs
        defExpr = view present defHistory
        currentClipboard = view clipboard appState
        currentEditState = view editState appState
        initiateRename = setEditState $ Naming initialRenameEditor
        initialRenameEditor = applyEdit gotoEOL $ editor EditorName (Just 1) $ fromMaybe "" maybeDefName
        commitName newName = continue $ appState
            & defs . ix defId . _1 .~ (if null newName then Nothing else Just newName)
            & editState .~ NotEditing
        navToParent = nav parentPath
        navToChild = nav pathToFirstChildOfSelected
        navBackward = nav prevSiblingPath
        navForward = nav nextSiblingPath
        nav path = case getItemAtPath path of
            Just itemAtPath -> liftIO getNewAppState >>= continue where
                getNewAppState = updateEvalResult $ appState & locationHistory . present . _DefView . _2 .~ path
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
        getItemAtPath path = getItemAtPathInExpr path defExpr
        goToDefinition = case selected of
            Expr (E.Def defId) | Map.member defId currentDefs -> goToDef defId appState
            _ -> continue appState
        initiateSelectionEdit = setEditState $ SelectionEditing initialSelectionEditor Nothing
        initialSelectionEditor = applyEdit gotoEOL $ editor EditorName (Just 1) initialSelectionEditorContent
        initialSelectionEditorContent = printAutocompleteItem defNames selected
        cancelEdit = setEditState NotEditing
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
        modifySelected modifyExpr modifyPattern = modifyAtPathInExpr selectionPath modifyExpr modifyPattern defExpr
        setEditState newEditState = continue $ appState & editState .~ newEditState
        callSelected = modifyExprContainingSelection $ \expr -> E.Call expr E.Hole
        applyFnToSelected = modifyExprContainingSelection $ E.Call E.Hole
        wrapSelectedInFn = modifyExprContainingSelection $ \expr -> E.Fn (pure (P.Wildcard, expr))
        modifyExprContainingSelection modify = liftIO createNewAppState >>= continue where
            createNewAppState = updateDerivedState $ appState
                & defs . ix defId . _2 %~ push newExpr
                & locationHistory . present . _DefView . _2 .~ pathToExprContainingSelection
                & editState .~ NotEditing
            newExpr = modifyAtPathInExpr pathToExprContainingSelection modify id defExpr
            pathToExprContainingSelection = dropPatternPartOfPath defExpr selectionPath
        addAlternativeToSelected = maybe (continue appState) modifyDef (addAlternativeAtPath selectionPath defExpr)
        deleteSelected = modifyDef $ replaceSelected E.Hole P.Wildcard
        modifyDef newExpr = liftIO createNewAppState >>= continue where
            createNewAppState = updateDerivedState $ appState
                & defs . ix defId . _2 %~ push newExpr
                & editState .~ NotEditing
        switchToNextWrappingStyle = modifyWrappingStyle getNext
        switchToPrevWrappingStyle = modifyWrappingStyle getPrev
        modifyWrappingStyle modify = continue $ appState & wrappingStyle %~ modify
        getNext current = if current == maxBound then minBound else succ current
        getPrev current = if current == minBound then maxBound else pred current
        copy = continue $ appState & clipboard %~ case selected of
            Expr e -> clipboardExpr .~ Just e
            Pattern p -> clipboardPattern .~ Just p
        paste = modifyDef $ modifySelected (maybe id const exprClipboard) (maybe id const patternClipboard)
        Clipboard exprClipboard patternClipboard = currentClipboard
        undo = liftIO (updateDerivedState $ appState & defs . ix defId . _2 %~ goBack) >>= continue
        redo = liftIO (updateDerivedState $ appState & defs . ix defId . _2 %~ goForward) >>= continue

goToDef :: DefId -> AppState -> EventM AppResourceName (Next AppState)
goToDef name = modifyLocationHistory (push $ DefView (name, []))

goBackInLocationHistory :: AppState -> EventM AppResourceName (Next AppState)
goBackInLocationHistory = modifyLocationHistory goBack

goForwardInLocationHistory :: AppState -> EventM AppResourceName (Next AppState)
goForwardInLocationHistory = modifyLocationHistory goForward

modifyLocationHistory :: (History Location -> History Location) -> AppState -> EventM AppResourceName (Next AppState)
modifyLocationHistory modify appState = liftIO (updateDerivedState $ appState & locationHistory %~ modify) >>= continue

printAutocompleteItem :: Map.Map DefId DefName -> Selectable -> String
printAutocompleteItem defNames item = case item of
    Expr (E.Def id) -> fromMaybe "unnamed" $ Map.lookup id defNames
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

getVarsAtPath :: E.Path -> E.Expr d -> [E.VarName]
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

modifyAtPathInExpr :: E.Path -> (E.Expr d -> E.Expr d) -> (P.Pattern -> P.Pattern) -> E.Expr d -> E.Expr d
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

dropPatternPartOfPath :: E.Expr d -> E.Path -> E.Path
dropPatternPartOfPath expr path = case path of
    [] -> []
    edge:restOfPath -> case expr of
        E.Fn alts -> if even edge then [] else edge : dropPatternPartOfPath (snd $ alts NonEmpty.!! div edge 2) restOfPath
        E.Call callee arg | edge == 0 -> 0 : dropPatternPartOfPath callee restOfPath
        E.Call callee arg | edge == 1 -> 1 : dropPatternPartOfPath arg restOfPath
        _ -> error "invalid path"

addAlternativeAtPath :: E.Path -> E.Expr d -> Maybe (E.Expr d)
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
