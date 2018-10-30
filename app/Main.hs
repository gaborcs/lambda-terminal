{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.DeepSeq
import Control.Exception hiding (TypeError)
import Control.Lens hiding (DefName)
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.List
import Data.List.HT (viewR)
import Data.Maybe
import Data.Text.Zipper
import System.Environment
import System.IO.Error
import System.Timeout
import Text.Read
import Brick hiding (Location)
import Brick.Widgets.Border
import Brick.Widgets.Edit
import Safe
import Diff
import Eval
import History
import PrettyPrintType
import PrettyPrintValue
import Primitive
import Util
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector as Vec
import qualified Brick.Types
import qualified Brick.Widgets.List as ListWidget
import qualified Graphics.Vty as Vty
import qualified Expr as E
import qualified Infer
import qualified Pattern as P
import qualified Type as T
import qualified Value as V

data AppState = AppState
    { _typeDefs :: Map.Map Id (Maybe Name, History TypeDef)
    , _exprDefs :: Map.Map Id (Maybe Name, History Expr)
    , _locationHistory :: History Location
    , _wrappingStyle :: WrappingStyle
    , _clipboard :: Clipboard
    , _editState :: EditState
    , _derivedState :: Maybe DerivedState
    }
data Clipboard = Clipboard
    { _clipboardExpr :: Maybe Expr
    , _clipboardPattern :: Maybe Pattern
    }
data DerivedState = DerivedState
    { _inferResult :: InferResult
    , _evalResult :: EvalResult
    }
type Id = Int
type Name = String
type TypeDef = T.TypeDef TypeDefKey
type TypeDefKey = Int
type Expr = E.Expr Id DataConstructorKey
type DataConstructorKey = T.DataConstructorKey TypeDefKey
type Alternative = E.Alternative Id DataConstructorKey
type Pattern = P.Pattern DataConstructorKey
type Type = T.Type TypeDefKey
type Value = V.Value DataConstructorKey
type InferResult = Infer.InferResult TypeDefKey
type TypeTree = Infer.TypeTree TypeDefKey
type TypeError = Infer.TypeError TypeDefKey
data AppResourceName = DefListName | EditorName | AutocompleteName | Viewport deriving (Eq, Ord, Show)
type AppWidget = Widget AppResourceName
data WrappingStyle = NoParens | OneWordPerLine | Parens deriving (Eq, Enum, Bounded)
data Location = DefListView (Maybe SelectedDefKey) | TypeDefView TypeDefKey | ExprDefView ExprDefViewLocation
type SelectedDefKey = DefKey
data DefKey = TypeDefKey TypeDefKey | ExprDefKey Id deriving Eq
type ExprDefViewLocation = (Id, Path)
data EditState = NotEditing | Naming EditorState | SelectionEditing EditorState (Maybe AutocompleteState)
type EditorState = Editor String AppResourceName
data AutocompleteState = AutocompleteState AutocompleteList EditorExtent
type AutocompleteList = ListWidget.List AppResourceName Selectable
type EditorExtent = Extent AppResourceName
data EvalResult = Timeout | Error | Value Value
data Selectable = Expr Expr | Pattern Pattern deriving Eq
newtype RenderChild = RenderChild (ChildIndex -> Renderer -> RenderResult)
type Renderer = WrappingStyle -> RenderChild -> RenderResult
type RenderResult = (RenderResultType, AppWidget)
data RenderResultType = OneWord | OneLine | MultiLine deriving Eq
data Selection = ContainsSelection Path | WithinSelection | NoSelection deriving Eq

makeLenses ''AppState
makeLenses ''Clipboard
makeLenses ''DerivedState
makePrisms ''Location
makePrisms ''Selectable

main :: IO AppState
main = getInitialState >>= defaultMain app

getInitialState :: IO AppState
getInitialState = do
    let typeDefs = Map.empty
    readExprDefsResult <- tryJust (guard . isDoesNotExistError) readExprDefs
    let exprDefs = either (const Map.empty) (Map.map $ fmap History.create) readExprDefsResult
    let clipboard = Clipboard Nothing Nothing
    let defKeys = (TypeDefKey <$> Map.keys typeDefs) ++ (ExprDefKey <$> Map.keys exprDefs)
    let locationHistory = History.create $ DefListView $ listToMaybe defKeys
    return $ AppState typeDefs exprDefs locationHistory NoParens clipboard NotEditing Nothing

readExprDefs :: IO (Map.Map Id (Maybe Name, Expr))
readExprDefs = do
    exprDefsPath <- getExprDefsPath
    content <- readFile exprDefsPath
    return $ Map.fromList $ read <$> lines content

writeExprDefs :: AppState -> IO ()
writeExprDefs appState = do
    exprDefsPath <- getExprDefsPath
    writeFile exprDefsPath $ unlines $ map show $ Map.toList $ fmap (view present) <$> view exprDefs appState

getExprDefsPath :: IO FilePath
getExprDefsPath = do
    maybeProjectPath <- getProjectPath
    return $ fromMaybe "" maybeProjectPath ++ "expr-defs"

getProjectPath :: IO (Maybe FilePath)
getProjectPath = listToMaybe <$> getArgs

app :: App AppState e AppResourceName
app = App
    { appDraw = draw
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap = const $ attrMap Vty.defAttr [] }

draw :: AppState -> [AppWidget]
draw appState = case view present $ view locationHistory appState of
    DefListView maybeSelectedDefKey -> drawDefListView appState maybeSelectedDefKey
    TypeDefView typeDefKey -> drawTypeDefView appState typeDefKey
    ExprDefView defViewLocation -> drawExprDefView appState defViewLocation

drawDefListView :: AppState -> Maybe SelectedDefKey -> [AppWidget]
drawDefListView appState maybeSelectedDefKey = [ renderTitle "Definitions" <=> renderedList ] where
    renderedList = vBox $ renderItem <$> getDefKeys appState
    renderItem key = grayIf (Just key /= maybeSelectedDefKey) (str $ getDefName appState key)
    grayIf cond = if cond then modifyDefAttr $ flip Vty.withForeColor gray else id

getDefName :: AppState -> DefKey -> Name
getDefName appState key = case key of
    TypeDefKey k -> getTypeName appState k
    ExprDefKey id -> getExprName appState id

getTypeName :: AppState -> TypeDefKey -> Name
getTypeName appState key = fromMaybe "Unnamed" $ fst $ view typeDefs appState Map.! key

getExprName :: AppState -> Id -> Name
getExprName appState id = fromMaybe "unnamed" $ fst $ view exprDefs appState Map.! id

gray :: Vty.Color
gray = Vty.rgbColor 128 128 128 -- this shade seems ok on both light and dark backgrounds

drawTypeDefView :: AppState -> TypeDefKey -> [AppWidget]
drawTypeDefView appState typeDefKey = [ title <=> vBox renderedDataConstructors ] where
    title = renderTitle $ getTypeName appState typeDefKey
    renderedDataConstructors = renderDataConstructor <$> dataConstructors
    renderDataConstructor (T.DataConstructor name paramTypes) = str $ unwords $ name : printedParamTypes where
        printedParamTypes = printParamType <$> paramTypes
        printParamType t = inParensIf (isMultiWord t) (prettyPrintType (getTypeName appState) varNames t)
    T.TypeDef varNames dataConstructors = getTypeDef (view typeDefs appState) typeDefKey

drawExprDefView :: AppState -> ExprDefViewLocation -> [AppWidget]
drawExprDefView appState (defId, selectionPath) = ui where
    AppState _ defs _ wrappingStyle _ editState (Just (DerivedState inferResult evalResult)) = appState
    ui = case editState of
        SelectionEditing _ (Just (AutocompleteState autocompleteList editorExtent)) | autocompleteListLength > 0 ->
            [ translateBy autocompleteOffset autocomplete, layout ] where
            autocompleteOffset = Brick.Types.Location (editorX, editorY + 1)
            Brick.Types.Location (editorX, editorY) = extentUpperLeft editorExtent
            autocomplete = hLimit 20 $ vLimit (min autocompleteListLength 5) $
                ListWidget.renderList renderAutocompleteItem True $ printAutocompleteItem (getExprName appState) <$> autocompleteList
            autocompleteListLength = length $ ListWidget.listElements autocompleteList
        _ -> [ layout ]
    layout = renderedTitle <=> viewport Viewport Both coloredExpr <=> str bottomStr
    renderedTitle = case editState of
        Naming editor -> prompt <+> renderEditor (str . head) True editor where
            prompt = str $ if isJust maybeDefName then "Rename to: " else "Name to: "
        _ -> renderTitle $ fromMaybe "unnamed" maybeDefName
    (maybeDefName, defHistory) = defs Map.! defId
    coloredExpr = modifyDefAttr (`Vty.withForeColor` gray) renderedExpr -- unselected parts of the expression are gray
    expr = view present defHistory
    (_, renderedExpr) = renderWithAttrs wrappingStyle editState (ContainsSelection selectionPath) maybeTypeError (renderExpr appState expr)
    maybeTypeError = preview Infer._TypeError inferResult
    maybeSelectionType = getTypeAtPathInInferResult selectionPath inferResult
    bottomStr = case maybeSelectionType of
        Just t -> evalStr ++ ": " ++ prettyPrintType (getTypeName appState) defaultTypeVarNames t
        Nothing -> "Type error"
    evalStr = case evalResult of
        Timeout -> "<eval timeout>"
        Error -> ""
        Value v -> fromMaybe "" $ prettyPrintValue (view T.constructorName) v

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
    SelectionEditing editor _ | selected -> (OneWord, visible . highlight $ hLimit (textWidth editStr + 1) renderedEditor) where
        editStr = head $ getEditContents editor
        renderedEditor = renderEditor (str . head) True editor
    _ -> (renderResultType, (if selected then visible . highlight else id) $ makeRedIfNeeded widget)
    where
        selected = selection == ContainsSelection []
        withinSelection = selection == WithinSelection
        makeRedIfNeeded = if hasError && (selected || withinSelection) then makeRed else id
        hasError = maybe False Infer.hasErrorAtRoot maybeTypeError
        makeRed = modifyDefAttr $ flip Vty.withForeColor Vty.red
        (renderResultType, widget) = renderer wrappingStyle $ RenderChild renderChild
        renderChild index = renderWithAttrs wrappingStyle editState (getChildSelection selection index) (getChildTypeError maybeTypeError index)

renderExpr :: AppState -> Expr -> Renderer
renderExpr appState expr wrappingStyle (RenderChild renderChild) = case expr of
    E.Hole -> (OneWord, str "_")
    E.Def id -> (OneWord, str $ getExprName appState id)
    E.Var var -> (OneWord, str var)
    E.Fn alternatives -> if null restOfAltResults then singleAltResult else multiAltResult where
        singleAltResult = (firstAltResultType, str "λ " <+> firstAltWidget)
        multiAltResult = (MultiLine, vBox $ str "λ " <+> firstAltWidget : map (str "| " <+>) restOfAltWidgets)
        (firstAltResultType, firstAltWidget) NonEmpty.:| restOfAltResults =
            NonEmpty.zipWith (renderAlternative appState $ RenderChild renderChild) (NonEmpty.fromList [0..]) alternatives
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
            (calleeResultType, renderedCallee) = renderChild 0 (renderExpr appState callee)
            (argResultType, renderedArg) = renderChild 1 (renderExpr appState arg)
    E.Constructor key -> (OneWord, str $ view T.constructorName key)
    E.Int n -> (OneWord, str $ show n)
    E.Primitive p -> (OneWord, str $ getDisplayName p)

withParensIf :: Bool -> Widget n -> Widget n
withParensIf cond w = if cond then str "(" <+> w <+> str ")" else w

renderAlternative :: AppState -> RenderChild -> Int -> Alternative -> RenderResult
renderAlternative appState (RenderChild renderChild) alternativeIndex (patt, expr) =
    if exprResultType == MultiLine
    then (MultiLine, renderedPattern <+> str " ->" <=> renderedExpr)
    else (OneLine, renderedPattern <+> str " -> " <+> renderedExpr)
    where
        (_, renderedPattern) = renderChild (2 * alternativeIndex) $ renderPattern patt
        (exprResultType, renderedExpr) = renderChild (2 * alternativeIndex + 1) $ renderExpr appState expr

renderPattern :: Pattern -> Renderer
renderPattern patt _ (RenderChild renderChild) = case patt of
    P.Wildcard -> (OneWord, str "_")
    P.Var var -> (OneWord, str var)
    P.Constructor key children -> (resultType, hBox $ intersperse (str " ") (str name : renderedChildren)) where
        resultType = if null children then OneWord else OneLine
        name = view T.constructorName key
        renderedChildren = addParensIfNeeded <$> zipWith renderChild [0..] childRenderers
        addParensIfNeeded (resultType, renderedChild) = withParensIf (resultType /= OneWord) renderedChild
        childRenderers = renderPattern <$> children
    P.Int n -> (OneWord, str $ show n)

indent :: Widget n -> Widget n
indent w = str "  " <+> w

highlight :: Widget n -> Widget n
highlight = modifyDefAttr $ const Vty.defAttr -- the gray foreground color is changed back to the default

getChildSelection :: Selection -> ChildIndex -> Selection
getChildSelection selection index = case selection of
    ContainsSelection (i:childPath) -> if i == index then ContainsSelection childPath else NoSelection
    ContainsSelection [] -> WithinSelection
    WithinSelection -> WithinSelection
    NoSelection -> NoSelection

getChildTypeError :: Maybe TypeError -> ChildIndex -> Maybe TypeError
getChildTypeError maybeTypeError index = childResult >>= preview Infer._TypeError
    where childResult = (!! index) <$> maybeTypeError

getTypeAtPathInInferResult :: Path -> InferResult -> Maybe Type
getTypeAtPathInInferResult path inferResult = case inferResult of
    Infer.Typed typeTree -> getTypeAtPathInTypeTree path typeTree
    Infer.TypeError childResults -> case path of
        [] -> Nothing
        index:restOfPath -> getTypeAtPathInInferResult restOfPath $ childResults !! index

getTypeAtPathInTypeTree :: Path -> TypeTree -> Maybe Type
getTypeAtPathInTypeTree path (Infer.TypeTree t children) = case path of
    [] -> Just t
    index:restOfPath -> getTypeAtPathInTypeTree restOfPath $ children !! index

getItemAtPathInExpr :: Path -> Expr -> Maybe Selectable
getItemAtPathInExpr path expr = getItemAtPathInSelectable path (Expr expr)

getItemAtPathInSelectable :: Path -> Selectable -> Maybe Selectable
getItemAtPathInSelectable path selectable = case path of
    [] -> Just selectable
    edge:restOfPath -> getChildInSelectable selectable edge >>= getItemAtPathInSelectable restOfPath

getChildInSelectable :: Selectable -> ChildIndex -> Maybe Selectable
getChildInSelectable selectable = case selectable of
    Expr e -> getChildInExpr e
    Pattern p -> getChildInPattern p

getChildInExpr :: Expr -> ChildIndex -> Maybe Selectable
getChildInExpr expr index = case (expr, index) of
    (E.Fn alternatives, _) -> do
        let altIndex = div index 2
        (patt, expr) <- getItemAtIndex altIndex (NonEmpty.toList alternatives)
        return $ if even index then Pattern patt else Expr expr
    (E.Call callee _, 0) -> Just $ Expr callee
    (E.Call _ arg, 1) -> Just $ Expr arg
    _ -> Nothing

getChildInPattern :: Pattern -> ChildIndex -> Maybe Selectable
getChildInPattern patt index = case patt of
    P.Constructor _ patterns -> Pattern <$> getItemAtIndex index patterns
    _ -> Nothing

handleEvent :: AppState -> BrickEvent n e -> EventM AppResourceName (Next AppState)
handleEvent appState brickEvent = case brickEvent of
    VtyEvent event -> case view present $ view locationHistory appState of
        DefListView maybeSelectedDefKey -> handleEventOnDefListView appState event maybeSelectedDefKey
        TypeDefView typeDefKey -> handleEventOnTypeDefView appState event typeDefKey
        ExprDefView location -> handleEventOnExprDefView appState event location
    _ -> continue appState

handleEventOnDefListView :: AppState -> Vty.Event -> Maybe SelectedDefKey -> EventM AppResourceName (Next AppState)
handleEventOnDefListView appState event maybeSelectedDefKey = case event of
    Vty.EvKey Vty.KEnter [] -> maybe continue goToDef maybeSelectedDefKey appState
    Vty.EvKey Vty.KUp [] -> maybe (continue appState) select maybePrevDefKey
    Vty.EvKey Vty.KDown [] -> maybe (continue appState) select maybeNextDefKey
    Vty.EvKey (Vty.KChar 'g') [] -> goBackInLocationHistory appState
    Vty.EvKey (Vty.KChar 'G') [] -> goForwardInLocationHistory appState
    Vty.EvKey (Vty.KChar 'A') [] -> addNewTypeDef
    Vty.EvKey (Vty.KChar 'a') [] -> addNewExprDef
    Vty.EvKey (Vty.KChar 'q') [] -> halt appState
    _ -> continue appState
    where
        select defKey = continue $ appState & locationHistory . present . _DefListView ?~ defKey
        maybePrevDefKey = fmap (subtract 1) maybeSelectedIndex >>= flip getItemAtIndex defKeys
        maybeNextDefKey = fmap (+ 1) maybeSelectedIndex >>= flip getItemAtIndex defKeys
        maybeSelectedIndex = maybeSelectedDefKey >>= flip elemIndex defKeys
        defKeys = getDefKeys appState
        addNewTypeDef = liftIO createNewAppState >>= continue where
            createNewAppState = handleDefsChange $ appState
                & typeDefs %~ Map.insert newDefId (Nothing, History.create $ T.TypeDef [] [])
                & locationHistory %~ push (TypeDefView newDefId) . (present . _DefListView ?~ ExprDefKey newDefId)
            newDefId = createNewDefId $ view typeDefs appState
        addNewExprDef = liftIO createNewAppState >>= continue where
            createNewAppState = handleDefsChange $ appState
                & exprDefs %~ Map.insert newDefId (Nothing, History.create E.Hole)
                & locationHistory %~ push (ExprDefView (newDefId, [])) . (present . _DefListView ?~ ExprDefKey newDefId)
            newDefId = createNewDefId $ view exprDefs appState
        createNewDefId defs = if null defs then 0 else fst (Map.findMax defs) + 1

getDefKeys :: AppState -> [DefKey]
getDefKeys appState = (TypeDefKey <$> getTypeDefKeys appState) ++ (ExprDefKey <$> getExprDefIds appState)

getTypeDefKeys :: AppState -> [TypeDefKey]
getTypeDefKeys = Map.keys . view typeDefs

getExprDefIds :: AppState -> [Id]
getExprDefIds = Map.keys . view exprDefs

handleEventOnTypeDefView :: AppState -> Vty.Event -> TypeDefKey -> EventM AppResourceName (Next AppState)
handleEventOnTypeDefView appState event _ = case event of
    Vty.EvKey (Vty.KChar 'g') [] -> goBackInLocationHistory appState
    Vty.EvKey (Vty.KChar 'G') [] -> goForwardInLocationHistory appState
    Vty.EvKey (Vty.KChar 'q') [] -> halt appState
    _ -> continue appState

handleEventOnExprDefView :: AppState -> Vty.Event -> ExprDefViewLocation -> EventM AppResourceName (Next AppState)
handleEventOnExprDefView appState event (defId, selectionPath) = case currentEditState of
    Naming editor -> case event of
        Vty.EvKey Vty.KEsc [] -> cancelEdit
        Vty.EvKey Vty.KEnter [] -> commitName $ head $ getEditContents editor
        _ -> do
            newEditor <- handleEditorEvent event editor
            continue $ appState & editState .~ Naming newEditor
    SelectionEditing editor maybeAutocompleteState -> case event of
        Vty.EvKey Vty.KEsc [] -> cancelEdit
        Vty.EvKey (Vty.KChar '\t') [] -> maybe (continue appState) commitAutocompleteSelection maybeAutocompleteState
        Vty.EvKey Vty.KEnter [] -> commitEditorContent editorContent
        _ -> do
            newEditor <- case event of
                -- ignore up/down as they are used to control the autocomplete
                Vty.EvKey Vty.KUp [] -> pure editor
                Vty.EvKey Vty.KDown [] -> pure editor
                _ -> handleEditorEvent event editor
            let newEditorContent = head $ getEditContents newEditor
            let editorContentChanged = newEditorContent /= editorContent
            let isSimilarToEditorContent str = map toLower newEditorContent `isInfixOf` map toLower str
            let isMatch = isSimilarToEditorContent . printAutocompleteItem getCurrentExprName
            newAutocompleteList <- case maybeAutocompleteState of
                Just (AutocompleteState autocompleteList _) | not editorContentChanged -> ListWidget.handleListEvent event autocompleteList
                _ -> pure $ ListWidget.list AutocompleteName items 1 where
                    items = Vec.fromList $ filter isMatch $ case selected of
                        Expr _ -> map Expr $ vars ++ primitives ++ defs ++ constructors where
                            vars = E.Var <$> getVarsAtPath selectionPath defExpr
                            primitives = E.Primitive <$> [minBound..]
                            defs = E.Def <$> exprDefIds
                            constructors = E.Constructor <$> constructorKeys
                            constructorKeys = typeDefKeys >>= getConstructorKeys
                            getConstructorKeys typeDefKey = T.DataConstructorKey typeDefKey <$> getConstructorNames typeDefKey
                            getConstructorNames typeDefKey = view T.dataConstructorName <$> getConstructors typeDefKey
                            getConstructors = view T.dataConstructors . getTypeDef currentCustomTypeDefs
                        Pattern _ -> map Pattern constructorPatterns where
                            constructorPatterns = do
                                typeDefKey <- typeDefKeys
                                T.DataConstructor constructorName paramTypes <-
                                    view T.dataConstructors $ getTypeDef currentCustomTypeDefs typeDefKey
                                let constructorKey = T.DataConstructorKey typeDefKey constructorName
                                let arity = length paramTypes
                                let wildcards = replicate arity P.Wildcard
                                return $ P.Constructor constructorKey wildcards
                    typeDefKeys = getTypeDefKeys appState
            maybeEditorExtent <- lookupExtent EditorName
            setEditState $ SelectionEditing newEditor $ AutocompleteState newAutocompleteList <$> maybeEditorExtent
        where editorContent = head $ getEditContents editor
    NotEditing -> case event of
        Vty.EvKey Vty.KEnter [] -> goToDefinition
        Vty.EvKey (Vty.KChar 'g') [] -> goBackInLocationHistory appState
        Vty.EvKey (Vty.KChar 'G') [] -> goForwardInLocationHistory appState
        Vty.EvKey (Vty.KChar 'N') [] -> initiateRenameDefinition
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
        Vty.EvKey Vty.KBS [] -> applyFnToSelected
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
        currentCustomTypeDefs = view typeDefs appState
        currentExprDefs = view exprDefs appState
        exprDefIds = Map.keys currentExprDefs
        getCurrentExprName = getExprName appState
        (maybeDefName, defHistory) = currentExprDefs Map.! defId
        defExpr = view present defHistory
        currentClipboard = view clipboard appState
        currentEditState = view editState appState
        initiateRenameDefinition = setEditState $ Naming initialRenameEditor
        initialRenameEditor = applyEdit gotoEOL $ editor EditorName (Just 1) $ fromMaybe "" maybeDefName
        commitName newName = continue $ case newName of
            firstChar : restOfChars | isLower firstChar && all isAlphaNum restOfChars -> appState
                & exprDefs . ix defId . _1 ?~ newName
                & editState .~ NotEditing
            "" -> appState
                & exprDefs . ix defId . _1 .~ Nothing
                & editState .~ NotEditing
            _ -> appState
        navToParent = nav parentPath
        navToChild = nav pathToFirstChildOfSelected
        navBackward = nav prevSiblingPath
        navForward = nav nextSiblingPath
        nav path = if isJust $ getItemAtPath path then liftIO getNewAppState >>= continue else continue appState where
            getNewAppState = updateEvalResult $ appState & locationHistory . present . _ExprDefView . _2 .~ path
        parentPath = if null selectionPath then [] else init selectionPath
        pathToFirstChildOfSelected = selectionPath ++ [0]
        prevSiblingPath = if null selectionPath then [] else init selectionPath ++ [mod (last selectionPath - 1) siblingCount]
        nextSiblingPath = if null selectionPath then [] else init selectionPath ++ [mod (last selectionPath + 1) siblingCount]
        siblingCount = case getItemAtPath (init selectionPath) of
            Just (Expr (E.Fn alts)) -> 2 * length alts
            Just (Expr (E.Call _ _)) -> 2
            Just (Pattern (P.Constructor _ siblings)) -> length siblings
            _ -> 1
        selected = fromJustNote "selection path should be a valid path" $ getItemAtPath selectionPath
        getItemAtPath path = getItemAtPathInExpr path defExpr
        goToDefinition = case selected of
            Expr (E.Def defId) | Map.member defId currentExprDefs -> goToExprDef defId appState
            _ -> continue appState
        initiateSelectionEdit = setEditState $ SelectionEditing initialSelectionEditor Nothing
        initialSelectionEditor = applyEdit gotoEOL $ editor EditorName (Just 1) initialSelectionEditorContent
        initialSelectionEditorContent = printAutocompleteItem getCurrentExprName selected
        cancelEdit = setEditState NotEditing
        commitEditorContent editorContent = case readMaybe editorContent of
            Just int -> modifyDef $ replaceSelected (E.Int int) (P.Int int)
            Nothing -> case editorContent of
                firstChar : restOfChars | isAlpha firstChar && all isAlphaNum restOfChars ->
                    modifyDef $ replaceSelected (E.Var editorContent) (P.Var editorContent)
                _ -> continue appState
        commitAutocompleteSelection (AutocompleteState autocompleteList _) = case ListWidget.listSelectedElement autocompleteList of
            Just (_, selectedItem) -> modifyDef $ case selectedItem of
                Expr expr -> modifySelected (const expr) id
                Pattern patt -> modifySelected id (const patt)
            _ -> continue appState
        replaceSelected replacementIfExpr replacementIfPattern = modifySelected (const replacementIfExpr) (const replacementIfPattern)
        modifySelected modifyExpr modifyPattern = modifyAtPathInExpr selectionPath modifyExpr modifyPattern defExpr
        setEditState newEditState = continue $ appState & editState .~ newEditState
        callSelected = modifyExprContainingSelection (`E.Call` E.Hole) [1]
        applyFnToSelected = modifyExprContainingSelection (E.Hole `E.Call`) [0]
        wrapSelectedInFn = modifyExprContainingSelection (\expr -> E.Fn $ pure (P.Wildcard, expr)) [0]
        addAlternativeToSelected = maybe (continue appState) addAlternative (getContainingFunction selectionPath defExpr)
        addAlternative (fnPath, alts) = modifyExpr fnPath (const $ E.Fn $ alts <> pure (P.Wildcard, E.Hole)) [2 * length alts]
        modifyExprContainingSelection = modifyExpr $ dropPatternPartOfPath defExpr selectionPath
        modifyExpr path modify selectionPathInModifiedExpr = liftIO createNewAppState >>= continue where
            createNewAppState = handleDefsChange $ appState
                & exprDefs . ix defId . _2 %~ push newExpr
                & locationHistory . present . _ExprDefView . _2 .~ newSelectionPath
            newExpr = modifyAtPathInExpr path modify id defExpr
            newSelectionPath = path ++ selectionPathInModifiedExpr
        deleteSelected = case selected of
            Expr E.Hole -> removeSelectedFromParent
            Pattern P.Wildcard -> removeSelectedFromParent
            _ -> modifyDef $ replaceSelected E.Hole P.Wildcard
        removeSelectedFromParent = case viewR selectionPath of
            Just (parentPath, childIndex) -> case getItemAtPathInExpr parentPath defExpr of
                Just (Expr parent) -> modifyExpr parentPath (const parentReplacement) selectionPathInParentReplacement where
                    (parentReplacement, selectionPathInParentReplacement) = case parent of
                        E.Fn alts -> case NonEmpty.nonEmpty $ removeItemAtIndex altIndex $ NonEmpty.toList alts of
                            Just altsWithoutSelected -> (E.Fn altsWithoutSelected, [newChildIndex]) where
                                newChildIndex = if altIndex == length altsWithoutSelected then childIndex - 2 else childIndex
                            Nothing -> (if selected == Pattern P.Wildcard then snd $ NonEmpty.head alts else E.Hole, [])
                        E.Call _ arg | childIndex == 0 -> (arg, [])
                        E.Call callee _ | childIndex == 1 -> (callee, [])
                        _ -> error "invalid path"
                    altIndex = div childIndex 2
                _ -> continue appState
            Nothing -> continue appState
        modifyDef newExpr = liftIO createNewAppState >>= continue where
            createNewAppState = handleDefsChange $ appState
                & exprDefs . ix defId . _2 %~ push newExpr
                & editState .~ NotEditing
        switchToNextWrappingStyle = modifyWrappingStyle getNext
        switchToPrevWrappingStyle = modifyWrappingStyle getPrev
        modifyWrappingStyle modify = continue $ appState & wrappingStyle %~ modify
        getNext current = if current == maxBound then minBound else succ current
        getPrev current = if current == minBound then maxBound else pred current
        copy = continue $ appState & clipboard %~ case selected of
            Expr e -> clipboardExpr ?~ e
            Pattern p -> clipboardPattern ?~ p
        paste = modifyDef $ modifySelected (maybe id const exprClipboard) (maybe id const patternClipboard)
        Clipboard exprClipboard patternClipboard = currentClipboard
        undo = modifyDefHistory goBack
        redo = modifyDefHistory goForward
        modifyDefHistory modify = liftIO createAppState >>= continue where
            createAppState = handleDefsChange $ appState
                & exprDefs . ix defId . _2 .~ newDefHistory
                & locationHistory . present . _ExprDefView . _2 %~ modifySelectionPath
            newDefHistory = modify defHistory
            modifySelectionPath = maybe id const $ getDiffPathBetweenExprs defExpr $ view present newDefHistory

goToDef :: DefKey -> AppState -> EventM AppResourceName (Next AppState)
goToDef key = case key of
    TypeDefKey k -> goToTypeDef k
    ExprDefKey id -> goToExprDef id

goToTypeDef :: TypeDefKey -> AppState -> EventM AppResourceName (Next AppState)
goToTypeDef key = modifyLocationHistory $ push $ TypeDefView key

goToExprDef :: Id -> AppState -> EventM AppResourceName (Next AppState)
goToExprDef id = modifyLocationHistory $ push $ ExprDefView (id, [])

goBackInLocationHistory :: AppState -> EventM AppResourceName (Next AppState)
goBackInLocationHistory = modifyLocationHistory goBack

goForwardInLocationHistory :: AppState -> EventM AppResourceName (Next AppState)
goForwardInLocationHistory = modifyLocationHistory goForward

modifyLocationHistory :: (History Location -> History Location) -> AppState -> EventM AppResourceName (Next AppState)
modifyLocationHistory modify appState = liftIO (handleDefsChange $ appState & locationHistory %~ modify) >>= continue

printAutocompleteItem :: (Id -> Name) -> Selectable -> String
printAutocompleteItem getExprName item = case item of
    Expr (E.Def id) -> getExprName id
    Expr (E.Var name) -> name
    Expr (E.Constructor key) -> view T.constructorName key
    Expr (E.Int n) -> show n
    Expr (E.Primitive p) -> getDisplayName p
    Pattern (P.Var name) -> name
    Pattern (P.Constructor key _) -> view T.constructorName key
    Pattern (P.Int n) -> show n
    _ -> ""

getVarsAtPath :: Path -> E.Expr d c -> [E.VarName]
getVarsAtPath path expr = case path of
    [] -> []
    edge:restOfPath -> case expr of
        E.Fn alts | odd edge -> getVars patt ++ getVarsAtPath restOfPath body where
            (patt, body) = alts NonEmpty.!! div edge 2
        E.Call callee _ | edge == 0 -> getVarsAtPath restOfPath callee
        E.Call _ arg | edge == 1 -> getVarsAtPath restOfPath arg
        _ -> error "invalid path"

getVars :: P.Pattern c -> [E.VarName]
getVars (P.Var name) = [name]
getVars (P.Constructor _ children) = children >>= getVars
getVars _ = []

modifyAtPathInExpr :: Path -> (E.Expr d c -> E.Expr d c) -> (P.Pattern c -> P.Pattern c) -> E.Expr d c -> E.Expr d c
modifyAtPathInExpr path modifyExpr modifyPattern expr = case path of
    [] -> modifyExpr expr
    edge:restOfPath -> case expr of
        E.Fn alts -> E.Fn $ alts & ix (div edge 2) %~ modifyAlt where
            modifyAlt (patt, expr) =
                if even edge
                then (modifyAtPathInPattern restOfPath modifyPattern patt, expr)
                else (patt, modifyAtPathInExpr restOfPath modifyExpr modifyPattern expr)
        E.Call callee arg | edge == 0 ->
            E.Call (modifyAtPathInExpr restOfPath modifyExpr modifyPattern callee) arg
        E.Call callee arg | edge == 1 ->
            E.Call callee (modifyAtPathInExpr restOfPath modifyExpr modifyPattern arg)
        _ -> error "invalid path"

modifyAtPathInPattern :: Path -> (P.Pattern t -> P.Pattern t) -> P.Pattern t -> P.Pattern t
modifyAtPathInPattern path modify patt = case path of
    [] -> modify patt
    edge:restOfPath -> case patt of
        P.Constructor name children -> P.Constructor name $ children & ix edge %~ modifyAtPathInPattern restOfPath modify
        _ -> error "invalid path"

dropPatternPartOfPath :: E.Expr d c -> Path -> Path
dropPatternPartOfPath expr path = case path of
    [] -> []
    edge:restOfPath -> case expr of
        E.Fn alts -> if even edge then [] else edge : dropPatternPartOfPath (snd $ alts NonEmpty.!! div edge 2) restOfPath
        E.Call callee _ | edge == 0 -> 0 : dropPatternPartOfPath callee restOfPath
        E.Call _ arg | edge == 1 -> 1 : dropPatternPartOfPath arg restOfPath
        _ -> error "invalid path"

getContainingFunction :: Path -> E.Expr d c -> Maybe (Path, NonEmpty.NonEmpty (E.Alternative d c))
getContainingFunction selectionPath expr = case (expr, selectionPath) of
    (E.Fn alts, edge:restOfSelectionPath) | odd edge -> case getContainingFunction restOfSelectionPath childAtEdge of
        Just (path, alts) -> Just (edge : path, alts)
        _ -> Just ([], alts)
        where childAtEdge = snd $ alts NonEmpty.!! div edge 2
    (E.Fn alts, _) -> Just ([], alts)
    (E.Call callee _, 0:restOfSelectionPath) -> (_1 %~ (0:)) <$> getContainingFunction restOfSelectionPath callee
    (E.Call _ arg, 1:restOfSelectionPath) -> (_1 %~ (1:)) <$> getContainingFunction restOfSelectionPath arg
    _ -> Nothing

handleDefsChange :: AppState -> IO AppState
handleDefsChange appState = do
    writeExprDefs appState
    updateDerivedState appState

updateDerivedState :: AppState -> IO AppState
updateDerivedState appState = do
    let getCurrentTypeDef = getTypeDef $ view typeDefs appState
    let currentExprDefs = view present . snd <$> view exprDefs appState
    let location = view present $ view locationHistory appState
    newDerivedState <- traverse (createDerivedState getCurrentTypeDef currentExprDefs) (preview _ExprDefView location)
    return $ appState & derivedState .~ newDerivedState

updateEvalResult :: AppState -> IO AppState
updateEvalResult appState = do
    let defExprs = view present . snd <$> view exprDefs appState
    let location = view present $ view locationHistory appState
    newEvalResult <- traverse (createEvalResult defExprs) (preview _ExprDefView location)
    return $ appState & derivedState . _Just . evalResult %~ flip fromMaybe newEvalResult

createDerivedState :: (TypeDefKey -> TypeDef) -> Map.Map Id Expr -> ExprDefViewLocation -> IO DerivedState
createDerivedState getTypeDef defs location@(exprName, _) =
    DerivedState (createInferResult getTypeDef defs exprName) <$> createEvalResult defs location

createInferResult :: (TypeDefKey -> TypeDef) -> Map.Map Id Expr -> Id -> InferResult
createInferResult getTypeDef defs defId = Infer.inferType (T.getConstructorType getTypeDef) defs $ defs Map.! defId

getTypeDef :: Map.Map Id (n, History TypeDef) -> TypeDefKey -> TypeDef
getTypeDef typeDefs key = view present . snd $ typeDefs Map.! key

createEvalResult :: Map.Map Id Expr -> ExprDefViewLocation -> IO EvalResult
createEvalResult defs (defId, selectionPath) = do
    let maybeDef = Map.lookup defId defs
    let maybeSelected = maybeDef >>= getItemAtPathInExpr selectionPath
    let maybeSelectedExpr = maybeSelected >>= preview _Expr
    let maybeSelectionValue = maybeSelectedExpr >>= eval defs
    timeoutResult <- timeout 10000 $ evaluate $ force maybeSelectionValue
    return $ case timeoutResult of
        Just (Just v) -> Value v
        Just Nothing -> Error
        Nothing -> Timeout
