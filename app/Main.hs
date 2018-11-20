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
    { _typeDefs :: Map.Map TypeDefKey (History TypeDef)
    , _committedExprDefs :: Map.Map ExprDefKey (History ExprDef) -- does not contain changes of any current edit
    , _committedLocations :: History Location -- does not contain selection changes of any current edit
    , _wrappingStyle :: WrappingStyle
    , _clipboard :: Clipboard
    , _editState :: EditState
    , _derivedState :: Maybe DerivedState
    }
data ExprDef = ExprDef
    { _name :: Maybe Name
    , _expr :: Expr
    } deriving (Read, Show)
data Clipboard = Clipboard
    { _clipboardExpr :: Maybe Expr
    , _clipboardPattern :: Maybe Pattern
    }
data EditState
    = NotEditing
    | Naming EditorState
    | AddingTypeConstructorParam ParamIndex EditorState
    | AddingDataConstructor DataConstructorIndex EditorState
    | AddingDataConstructorParam DataConstructorIndex ParamIndex EditorState
    | SelectionRenaming EditorState
    | EditingExpr Expr PathsToBeEdited EditorState (Maybe AutocompleteState)
type DataConstructorIndex = Int
type ParamIndex = Int
type PathsToBeEdited = NonEmpty.NonEmpty Path -- the first is the currently edited one
data DerivedState = DerivedState
    { _inferResult :: InferResult
    , _evalResult :: EvalResult
    }
type Name = String
type TypeDef = T.TypeDef TypeDefKey
type DataConstructor = T.DataConstructor TypeDefKey
type TypeDefKey = Int
type ExprDefKey = Int
type Expr = E.Expr ExprDefKey DataConstructorKey
type DataConstructorKey = T.DataConstructorKey TypeDefKey
type Alternative = E.Alternative ExprDefKey DataConstructorKey
type Pattern = P.Pattern DataConstructorKey
type Type = T.Type TypeDefKey
type Value = V.Value DataConstructorKey
type InferResult = Infer.InferResult TypeDefKey
type TypeTree = Infer.TypeTree TypeDefKey
type TypeError = Infer.TypeError TypeDefKey
data AppResourceName = DefListName | EditorName | AutocompleteName | Viewport deriving (Eq, Ord, Show)
type AppWidget = Widget AppResourceName
data WrappingStyle = NoParens | OneWordPerLine | Parens deriving (Eq, Enum, Bounded)
data Location = DefListView (Maybe SelectedDefKey) | TypeDefView TypeDefViewLocation | ExprDefView ExprDefViewLocation
type SelectedDefKey = DefKey
data DefKey = TypeDefKey TypeDefKey | ExprDefKey ExprDefKey deriving Eq
data TypeDefViewLocation = TypeDefViewLocation
    { _typeDefKey :: TypeDefKey
    , _typeDefViewSelection :: TypeDefViewSelection
    }
data TypeDefViewSelection
    = TypeConstructorSelection
        { _typeConstructorParamIndex :: Maybe Int
        }
    | DataConstructorSelection
        { _dataConstructorIndex :: Int
        , _dataConstructorParamIndex :: Maybe Int
        }
    deriving Eq
data ExprDefViewLocation = ExprDefViewLocation
    { _exprDefKey :: ExprDefKey
    , _exprDefViewSelection :: Path
    }
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
makeLenses ''ExprDef
makeLenses ''TypeDefViewLocation
makeLenses ''TypeDefViewSelection
makeLenses ''ExprDefViewLocation
makeLenses ''Clipboard
makeLenses ''DerivedState
makePrisms ''Location
makePrisms ''Selectable

main :: IO AppState
main = getInitialState >>= defaultMain app

getInitialState :: IO AppState
getInitialState = do
    readTypeDefsResult <- tryJust (guard . isDoesNotExistError) readTypeDefs
    readExprDefsResult <- tryJust (guard . isDoesNotExistError) readExprDefs
    let typeDefs = either (const Map.empty) (Map.map History.create) readTypeDefsResult
    let exprDefs = either (const Map.empty) (Map.map History.create) readExprDefsResult
    let clipboard = Clipboard Nothing Nothing
    let defKeys = (TypeDefKey <$> Map.keys typeDefs) ++ (ExprDefKey <$> Map.keys exprDefs)
    let locationHistory = History.create $ DefListView $ listToMaybe defKeys
    return $ AppState typeDefs exprDefs locationHistory NoParens clipboard NotEditing Nothing

readTypeDefs :: IO (Map.Map TypeDefKey TypeDef)
readTypeDefs = do
    path <- getTypeDefsPath
    content <- readFile path
    return $ Map.fromList $ read <$> lines content

writeTypeDefs :: AppState -> IO ()
writeTypeDefs appState = do
    path <- getTypeDefsPath
    writeFile path $ unlines $ map show $ Map.toList $ view present <$> view typeDefs appState

readExprDefs :: IO (Map.Map TypeDefKey ExprDef)
readExprDefs = do
    path <- getExprDefsPath
    content <- readFile path
    return $ Map.fromList $ read <$> lines content

writeExprDefs :: AppState -> IO ()
writeExprDefs appState = do
    path <- getExprDefsPath
    writeFile path $ unlines $ map show $ Map.toList $ view present <$> view committedExprDefs appState

getTypeDefsPath :: IO FilePath
getTypeDefsPath = do
    maybeProjectPath <- getProjectPath
    return $ fromMaybe "" maybeProjectPath ++ "type-defs"

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
draw appState = case getLocation appState of
    DefListView maybeSelectedDefKey -> drawDefListView appState maybeSelectedDefKey
    TypeDefView location -> drawTypeDefView appState location
    ExprDefView location -> drawExprDefView appState location

drawDefListView :: AppState -> Maybe SelectedDefKey -> [AppWidget]
drawDefListView appState maybeSelectedDefKey = [ renderTitle (str "Definitions") <=> renderedList ] where
    renderedList = toGray $ vBox $ renderItem <$> getDefKeys appState
    renderItem key = highlightIf (Just key == maybeSelectedDefKey) (str $ getDefName appState key)

getDefName :: AppState -> DefKey -> Name
getDefName appState key = case key of
    TypeDefKey k -> getTypeName appState k
    ExprDefKey k -> getExprName appState k

getTypeName :: AppState -> TypeDefKey -> Name
getTypeName appState key = fromMaybe "Unnamed" $ view (present . T.typeConstructor . T.typeConstructorName) $ view typeDefs appState Map.! key

getExprName :: AppState -> ExprDefKey -> Name
getExprName appState key = fromMaybe "unnamed" $ getExprDefs appState Map.! key ^. name

highlightIf :: Bool -> Widget n -> Widget n
highlightIf cond = if cond then highlight else id

toGray :: Widget n -> Widget n
toGray = modifyDefAttr $ flip Vty.withForeColor gray

gray :: Vty.Color
gray = Vty.rgbColor 128 128 128 -- this shade seems ok on both light and dark backgrounds

drawTypeDefView :: AppState -> TypeDefViewLocation -> [AppWidget]
drawTypeDefView appState (TypeDefViewLocation typeDefKey selection) = [ toGray $ renderedTitle <=> body ] where
    renderedTitle = case view editState appState of
        Naming editor -> str "Name: " <+> renderEditor (str . head) True editor
        _ -> renderTitle $ hBox $ intersperse (str " ") renderedTitleWords
    renderedTitleWords = case view editState appState of
        AddingTypeConstructorParam index editor -> str typeName : renderedParams where
            renderedParams = insertAt index (highlight $ renderExpandingSingleLineEditor editor) (str <$> typeConstructorParams)
        _ -> renderedTypeName : renderedParams where
            renderedTypeName = highlightIf (selection == TypeConstructorSelection Nothing) (str typeName)
            renderedParams = zipWith (highlightIf . isSelected) [0..] (str <$> typeConstructorParams)
            isSelected index =
                selection == TypeConstructorSelection Nothing || selection == TypeConstructorSelection (Just index)
    typeName = getTypeName appState typeDefKey
    typeConstructorParams = view T.typeConstructorParams typeConstructor
    body = vBox $ case view editState appState of
        AddingDataConstructor index editor -> insertAt index renderedEditor renderedDataConstructors where
            renderedEditor = renderEditor (str . head) True editor
        _ -> renderedDataConstructors
    renderedDataConstructors = zipWith renderDataConstructor [0..] dataConstructors
    renderDataConstructor dataConstructorIndex (T.DataConstructor name paramTypes) = highlightIf
        (selection == DataConstructorSelection dataConstructorIndex Nothing)
        (str name <+> str " " <+> hBox (intersperse (str " ") renderedParamTypes))
        where
            renderedParamTypes = zipWith (highlightIf . isSelected) [0..] $ case view editState appState of
                AddingDataConstructorParam constructorIndex paramIndex editor | constructorIndex == dataConstructorIndex ->
                    insertAt paramIndex (renderExpandingSingleLineEditor editor) $ renderParamType <$> paramTypes
                _ -> renderParamType <$> paramTypes
            renderParamType t = str $ inParensIf (isMultiWord t) (printType t)
            printType = prettyPrintType (getTypeName appState) (view T.typeConstructorParams typeConstructor)
            isSelected paramIndex = selection == DataConstructorSelection dataConstructorIndex (Just paramIndex)
    T.TypeDef typeConstructor dataConstructors = getPresentTypeDef (view typeDefs appState) typeDefKey

insertAt :: Int -> a -> [a] -> [a]
insertAt index item = (!! index) . iterate _tail %~ (item :)

renderExpandingSingleLineEditor :: (Ord n, Show n) => Editor String n -> Widget n
renderExpandingSingleLineEditor editor = hLimit (textWidth editStr + 1) $ renderEditor (str . head) True editor where
    editStr = head $ getEditContents editor

drawExprDefView :: AppState -> ExprDefViewLocation -> [AppWidget]
drawExprDefView appState (ExprDefViewLocation defKey selectionPath) = ui where
    AppState _ _ _ wrappingStyle _ editState (Just (DerivedState inferResult evalResult)) = appState
    ui = case editState of
        EditingExpr _ _ _ (Just (AutocompleteState autocompleteList editorExtent)) | autocompleteListLength > 0 ->
            [ translateBy autocompleteOffset autocomplete, layout ] where
            autocompleteOffset = Brick.Types.Location (editorX, editorY + 1)
            Brick.Types.Location (editorX, editorY) = extentUpperLeft editorExtent
            autocomplete = hLimit 20 $ vLimit (min autocompleteListLength 5) $
                ListWidget.renderList renderAutocompleteItem True $ printAutocompleteItem (getExprName appState) <$> autocompleteList
            autocompleteListLength = length $ ListWidget.listElements autocompleteList
        _ -> [ layout ]
    layout = renderedTitle <=> viewport Viewport Both coloredExpr <=> str bottomStr
    renderedTitle = case editState of
        Naming editor -> str "Name: " <+> renderEditor (str . head) True editor
        _ -> renderTitle $ str $ getExprName appState defKey
    coloredExpr = modifyDefAttr (`Vty.withForeColor` gray) renderedExpr -- unselected parts of the expression are gray
    def = getExprDefs appState Map.! defKey
    renderedExpr = snd $ renderWithAttrs wrappingStyle editState (ContainsSelection selectionPath) maybeTypeError renderer
    renderer = renderExpr appState $ view expr def
    maybeTypeError = preview Infer._TypeError inferResult
    maybeSelectionType = getTypeAtPathInInferResult selectionPath inferResult
    bottomStr = case maybeSelectionType of
        Just t -> evalStr ++ ": " ++ prettyPrintType (getTypeName appState) defaultTypeVarNames t
        Nothing -> "Type error"
    evalStr = case evalResult of
        Timeout -> "<eval timeout>"
        Error -> ""
        Value v -> fromMaybe "" $ prettyPrintValue (view T.constructorName) v

renderTitle :: Widget n -> Widget n
renderTitle title = hBorderWithLabel $ str "  " <+> title <+> str "  "

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
    EditingExpr _ _ editor _ | selected -> renderSelectionEditor editor
    SelectionRenaming editor | selected -> renderSelectionEditor editor
    _ -> (renderResultType, (if selected then visible . highlight else id) $ makeRedIfNeeded widget)
    where
        selected = selection == ContainsSelection []
        withinSelection = selection == WithinSelection
        makeRedIfNeeded = if hasError && (selected || withinSelection) then makeRed else id
        hasError = maybe False Infer.hasErrorAtRoot maybeTypeError
        makeRed = modifyDefAttr $ flip Vty.withForeColor Vty.red
        (renderResultType, widget) = renderer wrappingStyle $ RenderChild renderChild
        renderChild index = renderWithAttrs wrappingStyle editState (getChildSelection selection index) (getChildTypeError maybeTypeError index)
        renderSelectionEditor editor = (OneWord, visible . highlight $ renderExpandingSingleLineEditor editor)

renderExpr :: AppState -> Expr -> Renderer
renderExpr appState expr wrappingStyle (RenderChild renderChild) = case expr of
    E.Hole -> (OneWord, str "_")
    E.Def key -> (OneWord, str $ getExprName appState key)
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
    VtyEvent event -> case getLocation appState of
        DefListView maybeSelectedDefKey -> handleEventOnDefListView appState event maybeSelectedDefKey
        TypeDefView location -> handleEventOnTypeDefView appState event location
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
        select defKey = continue $ appState & committedLocations . present . _DefListView ?~ defKey
        maybePrevDefKey = fmap (subtract 1) maybeSelectedIndex >>= flip getItemAtIndex defKeys
        maybeNextDefKey = fmap (+ 1) maybeSelectedIndex >>= flip getItemAtIndex defKeys
        maybeSelectedIndex = maybeSelectedDefKey >>= flip elemIndex defKeys
        defKeys = getDefKeys appState
        addNewTypeDef = liftIO createNewAppState >>= continue where
            createNewAppState = handleTypeDefsChange $ appState
                & typeDefs %~ Map.insert newDefKey (History.create $ T.TypeDef (T.TypeConstructor Nothing []) [])
                & committedLocations %~ push newLocation . selectNewDef
            newLocation = TypeDefView $ TypeDefViewLocation newDefKey $ TypeConstructorSelection Nothing
            newDefKey = createNewDefKey $ view typeDefs appState
            selectNewDef = present . _DefListView ?~ TypeDefKey newDefKey
        addNewExprDef = liftIO createNewAppState >>= continue where
            createNewAppState = handleExprDefsChange $ appState
                & committedExprDefs %~ Map.insert newDefKey (History.create $ ExprDef Nothing E.Hole)
                & committedLocations %~ push newLocation . selectNewDef
            newLocation = ExprDefView $ ExprDefViewLocation newDefKey []
            newDefKey = createNewDefKey $ view committedExprDefs appState
            selectNewDef = present . _DefListView ?~ ExprDefKey newDefKey
        createNewDefKey defs = if null defs then 0 else fst (Map.findMax defs) + 1

getDefKeys :: AppState -> [DefKey]
getDefKeys appState = (TypeDefKey <$> getTypeDefKeys appState) ++ (ExprDefKey <$> getExprDefKeys appState)

getTypeDefKeys :: AppState -> [TypeDefKey]
getTypeDefKeys = Map.keys . view typeDefs

getExprDefKeys :: AppState -> [ExprDefKey]
getExprDefKeys = Map.keys . view committedExprDefs

handleEventOnTypeDefView :: AppState -> Vty.Event -> TypeDefViewLocation -> EventM AppResourceName (Next AppState)
handleEventOnTypeDefView appState event (TypeDefViewLocation typeDefKey selection) = case view editState appState of
    NotEditing -> case event of
        Vty.EvKey (Vty.KChar 'g') [] -> goBackInLocationHistory appState
        Vty.EvKey (Vty.KChar 'G') [] -> goForwardInLocationHistory appState
        Vty.EvKey (Vty.KChar 'N') [] -> initiateRenameDefinition appState
        Vty.EvKey Vty.KLeft [] -> navBackward
        Vty.EvKey Vty.KRight [] -> navForward
        Vty.EvKey Vty.KUp [] -> navToParent
        Vty.EvKey Vty.KDown [] -> navToChild
        Vty.EvKey (Vty.KChar 'j') [] -> navBackward
        Vty.EvKey (Vty.KChar 'l') [] -> navForward
        Vty.EvKey (Vty.KChar 'i') [] -> navToParent
        Vty.EvKey (Vty.KChar 'k') [] -> navToChild
        Vty.EvKey (Vty.KChar 'o') [] -> initiateAddDataConstructorBelowSelection
        Vty.EvKey (Vty.KChar 'O') [] -> initiateAddDataConstructorAboveSelection
        Vty.EvKey (Vty.KChar ' ') [] -> case selection of
            TypeConstructorSelection Nothing -> initiateAddTypeConstructorParam appState typeConstructorParamCount
            TypeConstructorSelection (Just paramIndex) -> initiateAddTypeConstructorParam appState $ paramIndex + 1
            DataConstructorSelection dataConstructorIndex Nothing ->
                initiateAddParamToDataConstructor appState dataConstructorIndex 0
            DataConstructorSelection dataConstructorIndex (Just paramIndex) ->
                initiateAddParamToDataConstructor appState dataConstructorIndex (paramIndex + 1)
        Vty.EvKey (Vty.KChar 'u') [] -> undo
        Vty.EvKey (Vty.KChar 'r') [] -> redo
        Vty.EvKey (Vty.KChar 'q') [] -> halt appState
        _ -> continue appState
    Naming editor -> case event of
        Vty.EvKey Vty.KEsc [] -> cancelEdit appState
        Vty.EvKey Vty.KEnter [] -> commitDefName appState (head $ getEditContents editor) isValidTypeName
        _ -> handleEditorEvent event editor >>= setEditState appState . Naming
    AddingTypeConstructorParam index editor -> case event of
        Vty.EvKey Vty.KEsc [] -> cancelEdit appState
        Vty.EvKey Vty.KEnter [] -> commitAddTypeConstructorParam appState typeDefKey index (head $ getEditContents editor)
        _ -> handleEditorEvent event editor >>= setEditState appState . AddingTypeConstructorParam index
    AddingDataConstructor index editor -> case event of
        Vty.EvKey Vty.KEsc [] -> cancelEdit appState
        Vty.EvKey Vty.KEnter [] -> commitAddDataConstructor appState typeDefKey index (head $ getEditContents editor)
        _ -> handleEditorEvent event editor >>= setEditState appState . AddingDataConstructor index
    AddingDataConstructorParam dataConstructorIndex paramIndex editor -> case event of
        Vty.EvKey Vty.KEsc [] -> cancelEdit appState
        Vty.EvKey Vty.KEnter [] ->
            if editorContent == "" || editorContent == "_"
            then commitAddParamToDataConstructor appState typeDefKey dataConstructorIndex paramIndex T.Wildcard
            else continue appState
            where editorContent = head $ getEditContents editor
        _ -> do
            newEditor <- handleEditorEvent event editor
            let newEditorContent = head $ getEditContents newEditor
            if newEditorContent == "λ" || newEditorContent == "\\"
            then commitAddParamToDataConstructor appState typeDefKey dataConstructorIndex paramIndex T.Fn
            else setEditState appState $ AddingDataConstructorParam dataConstructorIndex paramIndex newEditor
    _ -> continue appState
    where
        navBackward = setSelection $ case selection of
            TypeConstructorSelection Nothing -> TypeConstructorSelection Nothing
            TypeConstructorSelection (Just paramIndex) -> TypeConstructorSelection $ Just $ max (paramIndex - 1) 0
            DataConstructorSelection dataConstructorIndex Nothing ->
                if dataConstructorIndex > 0
                then DataConstructorSelection (dataConstructorIndex - 1) Nothing
                else TypeConstructorSelection Nothing
            DataConstructorSelection dataConstructorIndex (Just paramIndex) ->
                DataConstructorSelection dataConstructorIndex $ Just $ max (paramIndex - 1) 0
        navForward = setSelection $ case selection of
            TypeConstructorSelection Nothing ->
                if dataConstructorCount > 0
                then DataConstructorSelection 0 Nothing
                else TypeConstructorSelection Nothing
            TypeConstructorSelection (Just paramIndex) ->
                TypeConstructorSelection $ Just $ min (paramIndex + 1) (typeConstructorParamCount - 1)
            DataConstructorSelection dataConstructorIndex Nothing ->
                DataConstructorSelection (min (dataConstructorIndex + 1) (dataConstructorCount - 1)) Nothing
            DataConstructorSelection dataConstructorIndex (Just paramIndex) ->
                DataConstructorSelection dataConstructorIndex $ Just $ min (paramIndex + 1) (paramCount - 1) where
                    paramCount = length $ dataConstructors ^. ix dataConstructorIndex . T.dataConstructorParamTypes
        navToParent = setSelection $ case selection of
            TypeConstructorSelection _ -> TypeConstructorSelection Nothing
            DataConstructorSelection index _ -> DataConstructorSelection index Nothing
        navToChild = setSelection $ case selection of
            TypeConstructorSelection Nothing ->
                TypeConstructorSelection $ if typeConstructorParamCount > 0 then Just 0 else Nothing
            TypeConstructorSelection (Just paramIndex) -> TypeConstructorSelection (Just paramIndex)
            DataConstructorSelection index Nothing -> DataConstructorSelection index (Just 0)
            DataConstructorSelection index (Just paramIndex) -> DataConstructorSelection index (Just paramIndex)
        setSelection newSelection = continue $ appState
            & committedLocations . present . _TypeDefView . typeDefViewSelection .~ newSelection
        typeConstructorParamCount = length typeConstructorParams
        typeConstructorParams = appState ^. typeDefs . ix typeDefKey . present . T.typeConstructor . T.typeConstructorParams
        dataConstructorCount = length dataConstructors
        dataConstructors = getDataConstructors appState typeDefKey
        initiateAddDataConstructorBelowSelection = initiateAddDataConstructor appState $ case selection of
            TypeConstructorSelection _ -> 0
            DataConstructorSelection index _ -> index + 1
        initiateAddDataConstructorAboveSelection = initiateAddDataConstructor appState $ case selection of
            TypeConstructorSelection _ -> 0
            DataConstructorSelection index _ -> index
        undo = modifyTypeDefs appState $ ix typeDefKey %~ goBack
        redo = modifyTypeDefs appState $ ix typeDefKey %~ goForward

initiateAddParamToDataConstructor :: AppState -> Int -> Int -> EventM AppResourceName (Next AppState)
initiateAddParamToDataConstructor appState dataConstructorIndex paramIndex =
    setEditState appState $ AddingDataConstructorParam dataConstructorIndex paramIndex emptyEditor

commitAddParamToDataConstructor :: AppState -> TypeDefKey -> Int -> Int -> Type -> EventM AppResourceName (Next AppState)
commitAddParamToDataConstructor appState typeDefKey dataConstructorIndex paramIndex t = appState
    & editState .~ NotEditing
    & committedLocations . present . _TypeDefView . typeDefViewSelection . dataConstructorParamIndex ?~ paramIndex
    & modifyTypeDef typeDefKey
        (T.dataConstructors . ix dataConstructorIndex . T.dataConstructorParamTypes %~ insertAt paramIndex t)

emptyEditor :: Editor String AppResourceName
emptyEditor = editor EditorName (Just 1) ""

getDataConstructors :: AppState -> TypeDefKey -> [DataConstructor]
getDataConstructors appState typeDefKey = view (typeDefs . ix typeDefKey . present . T.dataConstructors) appState

setEditState :: AppState -> EditState -> EventM AppResourceName (Next AppState)
setEditState appState newEditState = continue $ appState & editState .~ newEditState

handleEventOnExprDefView :: AppState -> Vty.Event -> ExprDefViewLocation -> EventM AppResourceName (Next AppState)
handleEventOnExprDefView appState event (ExprDefViewLocation defKey selectionPath) = case currentEditState of
    Naming editor -> case event of
        Vty.EvKey Vty.KEsc [] -> cancelEdit appState
        Vty.EvKey Vty.KEnter [] -> commitDefName appState (head $ getEditContents editor) isValidExprName
        _ -> handleEditorEvent event editor >>= setEditState appState . Naming
    SelectionRenaming editor -> case event of
        Vty.EvKey Vty.KEsc [] -> cancelEdit appState
        Vty.EvKey Vty.KEnter [] -> commitSelectionRename $ head $ getEditContents editor
        _ -> do
            newEditor <- handleEditorEvent event editor
            setEditState appState $ SelectionRenaming newEditor
    EditingExpr editedExpr pathsToBeEdited editor maybeAutocompleteState -> case event of
        Vty.EvKey Vty.KEsc [] -> cancelEdit appState
        Vty.EvKey (Vty.KChar '\t') [] -> maybe (continue appState) (commitAutocompleteSelection editedExpr pathsToBeEdited) maybeAutocompleteState
        Vty.EvKey Vty.KEnter [] -> commitEditorContent editedExpr pathsToBeEdited editorContent
        _ -> do
            newEditor <- case event of
                -- ignore up/down as they are used to control the autocomplete
                Vty.EvKey Vty.KUp [] -> pure editor
                Vty.EvKey Vty.KDown [] -> pure editor
                _ -> handleEditorEvent event editor
            let newEditorContent = head $ getEditContents newEditor
            if newEditorContent == "λ" || newEditorContent == "\\"
            then editExprContainingSelection (const $ E.Fn $ pure (P.Wildcard, E.Hole)) ([0] NonEmpty.:| [[1]])
            else do
                let editorContentChanged = newEditorContent /= editorContent
                let isSimilarToEditorContent str = map toLower newEditorContent `isInfixOf` map toLower str
                let isMatch = isSimilarToEditorContent . printAutocompleteItem getCurrentExprName
                newAutocompleteList <- case maybeAutocompleteState of
                    Just (AutocompleteState autocompleteList _) | not editorContentChanged -> ListWidget.handleListEvent event autocompleteList
                    _ -> pure $ ListWidget.list AutocompleteName items 1 where
                        items = Vec.fromList $ filter isMatch $ case selected of
                            Expr _ -> map Expr $ vars ++ primitives ++ defs ++ constructors where
                                vars = E.Var <$> getVarsAtPath selectionPath (view expr def)
                                primitives = E.Primitive <$> [minBound..]
                                defs = E.Def <$> exprDefKeys
                                constructors = E.Constructor <$> constructorKeys
                                constructorKeys = typeDefKeys >>= getConstructorKeys
                                getConstructorKeys typeDefKey = T.DataConstructorKey typeDefKey <$> getConstructorNames typeDefKey
                                getConstructorNames typeDefKey = view T.dataConstructorName <$> getConstructors typeDefKey
                                getConstructors = view T.dataConstructors . getPresentTypeDef currentCustomTypeDefs
                            Pattern _ -> map Pattern constructorPatterns where
                                constructorPatterns = do
                                    typeDefKey <- typeDefKeys
                                    T.DataConstructor constructorName paramTypes <-
                                        view T.dataConstructors $ getPresentTypeDef currentCustomTypeDefs typeDefKey
                                    let constructorKey = T.DataConstructorKey typeDefKey constructorName
                                    let arity = length paramTypes
                                    let wildcards = replicate arity P.Wildcard
                                    return $ P.Constructor constructorKey wildcards
                        typeDefKeys = getTypeDefKeys appState
                maybeEditorExtent <- lookupExtent EditorName
                let newAutocompleteState = AutocompleteState newAutocompleteList <$> maybeEditorExtent
                setEditState appState $ EditingExpr editedExpr pathsToBeEdited newEditor newAutocompleteState
        where editorContent = head $ getEditContents editor
    NotEditing -> case event of
        Vty.EvKey Vty.KEnter [] -> goToDefinition
        Vty.EvKey (Vty.KChar 'g') [] -> goBackInLocationHistory appState
        Vty.EvKey (Vty.KChar 'G') [] -> goForwardInLocationHistory appState
        Vty.EvKey (Vty.KChar 'N') [] -> initiateRenameDefinition appState
        Vty.EvKey (Vty.KChar 'n') [] -> initiateRenameSelection
        Vty.EvKey Vty.KUp [] -> navToParent
        Vty.EvKey Vty.KDown [] -> navToChild
        Vty.EvKey Vty.KLeft [] -> navBackward
        Vty.EvKey Vty.KRight [] -> navForward
        Vty.EvKey (Vty.KChar 'i') [] -> navToParent
        Vty.EvKey (Vty.KChar 'k') [] -> navToChild
        Vty.EvKey (Vty.KChar 'j') [] -> navBackward
        Vty.EvKey (Vty.KChar 'l') [] -> navForward
        Vty.EvKey (Vty.KChar 'e') [] -> initiateSelectionEdit
        Vty.EvKey (Vty.KChar ')') [] -> callSelected
        Vty.EvKey (Vty.KChar '(') [] -> applyFnToSelected
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
    _ -> error "invalid state"
    where
        currentCustomTypeDefs = view typeDefs appState
        currentExprDefs = getExprDefs appState
        exprDefKeys = Map.keys currentExprDefs
        getCurrentExprName = getExprName appState
        def = currentExprDefs Map.! defKey
        currentClipboard = view clipboard appState
        currentEditState = view editState appState
        navToParent = nav parentPath
        navToChild = nav pathToFirstChildOfSelected
        navBackward = nav prevSiblingPath
        navForward = nav nextSiblingPath
        nav path = if isJust $ getItemAtPath path then liftIO createNewAppState >>= continue else continue appState where
            createNewAppState = updateEvalResult $ appState
                & committedLocations . present . _ExprDefView . exprDefViewSelection .~ path
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
        getItemAtPath path = getItemAtPathInExpr path (view expr def)
        goToDefinition = case selected of
            Expr (E.Def defKey) | Map.member defKey currentExprDefs -> goToExprDef defKey appState
            _ -> continue appState
        initiateRenameSelection = case selected of
            Pattern (P.Var name) -> setEditState appState $ SelectionRenaming $ applyEdit gotoEOL $ editor EditorName (Just 1) name
            Expr (E.Var name) -> setEditState appState $ SelectionRenaming $ applyEdit gotoEOL $ editor EditorName (Just 1) name
            _ -> continue appState
        initiateSelectionEdit = setEditState appState $ EditingExpr (view expr def) (pure selectionPath) initialSelectionEditor Nothing
        initialSelectionEditor = applyEdit gotoEOL $ editor EditorName (Just 1) initialSelectionEditorContent
        initialSelectionEditorContent = printAutocompleteItem getCurrentExprName selected
        commitSelectionRename editorContent = case selected of
            Pattern (P.Var name) | isValidVarName editorContent -> setExpr $ E.renameVar name editorContent (view expr def)
            Expr (E.Var name) | isValidVarName editorContent -> setExpr $ E.renameVar name editorContent (view expr def)
            _ -> continue appState
        commitEditorContent editedExpr (path NonEmpty.:| furtherPathsToBeEdited) editorContent = case readMaybe editorContent of
            Just int -> commitEdit path furtherPathsToBeEdited newExpr where
                newExpr = modifyAtPathInExpr path (const $ E.Int int) (const $ P.Int int) editedExpr
            _ | editorContent == "" || editorContent == "_" -> commitEdit path furtherPathsToBeEdited newExpr where
                newExpr = modifyAtPathInExpr path (const E.Hole) (const P.Wildcard) editedExpr
            _ | isValidVarName editorContent -> commitEdit path furtherPathsToBeEdited newExpr where
                newExpr = modifyAtPathInExpr path (const $ E.Var editorContent) (const $ P.Var editorContent) editedExpr
            _ -> continue appState
        commitAutocompleteSelection editedExpr (path NonEmpty.:| furtherPathsToBeEdited) (AutocompleteState autocompleteList _) =
            case ListWidget.listSelectedElement autocompleteList of
                Just (_, selectedItem) -> case selectedItem of
                    Expr expr -> commitEdit path furtherPathsToBeEdited newExpr where
                        newExpr = modifyAtPathInExpr path (const expr) id editedExpr
                    Pattern patt -> commitEdit path (newPathsToBeEdited ++ furtherPathsToBeEdited) newExpr where
                        newPathsToBeEdited = (path ++) <$> getWildcardPaths patt
                        newExpr = modifyAtPathInExpr path id (const patt) editedExpr
                _ -> continue appState
        commitEdit newSelectionPath furtherPathsToBeEdited newExpr = appState
            & editState .~ case NonEmpty.nonEmpty furtherPathsToBeEdited of
                Just paths -> EditingExpr newExpr paths emptyEditor Nothing
                Nothing -> NotEditing
            & committedLocations . present . _ExprDefView . exprDefViewSelection .~ newSelectionPath
            & modifyExprDef defKey (expr .~ newExpr)
        replaceSelected replacementIfExpr replacementIfPattern = modifySelected (const replacementIfExpr) (const replacementIfPattern)
        modifySelected modifyExpr modifyPattern = modifyAtPathInExpr selectionPath modifyExpr modifyPattern (view expr def)
        callSelected = editExprContainingSelection (`E.Call` E.Hole) (pure [1])
        applyFnToSelected = editExprContainingSelection (E.Hole `E.Call`) (pure [0])
        wrapSelectedInFn = editExprContainingSelection (\expr -> E.Fn $ pure (P.Wildcard, expr)) pathsToBeEdited where
            pathsToBeEdited = if isWrappingHole then pathToWildcard NonEmpty.:| [pathToHole] else pure pathToWildcard
            isWrappingHole = getItemAtPathInExpr pathToExprContainingSelection (view expr def) == Just (Expr E.Hole)
            pathToWildcard = [0]
            pathToHole = [1]
        editExprContainingSelection = editExpr pathToExprContainingSelection
        editExpr modificationPath modify pathsToBeEditedFromModificationPath =
            liftIO (updateDerivedState newAppState) >>= continue where
                newAppState = appState & editState .~ newEditState
                newEditState = EditingExpr editedExpr pathsToBeEdited emptyEditor Nothing
                editedExpr = modifyAtPathInExpr modificationPath modify id (view expr def)
                pathsToBeEdited = (modificationPath ++) <$> pathsToBeEditedFromModificationPath
        pathToExprContainingSelection = dropPatternPartOfPath (view expr def) selectionPath
        addAlternativeToSelected = maybe (continue appState) addAlternative (getContainingFunction selectionPath $ view expr def)
        addAlternative (fnPath, alts) = editExpr fnPath modify pathsToBeEditedFromFnPath where
            modify = const $ E.Fn $ alts <> pure (P.Wildcard, E.Hole)
            pathsToBeEditedFromFnPath = pathToWildcard NonEmpty.:| [pathToHole]
            pathToWildcard = [2 * newAltIndex]
            pathToHole = [2 * newAltIndex + 1]
            newAltIndex = length alts
        modifyExprAtPath path modify selectionPathInModifiedExpr = appState
            & committedLocations . present . _ExprDefView . exprDefViewSelection .~ path ++ selectionPathInModifiedExpr
            & modifyExprDef defKey (expr %~ modifyAtPathInExpr path modify id)
        deleteSelected = case selected of
            Expr E.Hole -> removeSelectedFromParent
            Pattern P.Wildcard -> removeSelectedFromParent
            _ -> setExpr $ replaceSelected E.Hole P.Wildcard
        removeSelectedFromParent = case viewR selectionPath of
            Just (parentPath, childIndex) -> case getItemAtPathInExpr parentPath (view expr def) of
                Just (Expr parent) -> modifyExprAtPath parentPath (const parentReplacement) selectionPathInParentReplacement where
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
        setExpr newExpr = modifyExprDef defKey (expr .~ newExpr) (appState & editState .~ NotEditing)
        switchToNextWrappingStyle = modifyWrappingStyle getNext
        switchToPrevWrappingStyle = modifyWrappingStyle getPrev
        modifyWrappingStyle modify = continue $ appState & wrappingStyle %~ modify
        getNext current = if current == maxBound then minBound else succ current
        getPrev current = if current == minBound then maxBound else pred current
        copy = continue $ appState & clipboard %~ case selected of
            Expr e -> clipboardExpr ?~ e
            Pattern p -> clipboardPattern ?~ p
        paste = setExpr $ modifySelected (maybe id const exprClipboard) (maybe id const patternClipboard)
        Clipboard exprClipboard patternClipboard = currentClipboard
        undo = modifyDefHistory goBack
        redo = modifyDefHistory goForward
        modifyDefHistory modify = liftIO createAppState >>= continue where
            createAppState = handleExprDefsChange $ appState
                & committedExprDefs . ix defKey .~ newDefHistory
                & committedLocations . present . _ExprDefView . exprDefViewSelection %~ modifySelectionPath
            newDefHistory = modify $ view committedExprDefs appState Map.! defKey
            modifySelectionPath = maybe id const $ getDiffPathBetweenExprs (view expr def) $ view (present . expr) newDefHistory

getWildcardPaths :: Pattern -> [Path]
getWildcardPaths patt = case patt of
    P.Wildcard -> [[]]
    P.Constructor _ children -> join $ zipWith (fmap . (:)) [0..] $ getWildcardPaths <$> children
    _ -> []

isValidTypeName :: Name -> Bool
isValidTypeName name = case name of
    firstChar : restOfChars | isUpper firstChar && all isAlphaNum restOfChars -> True
    _ -> False

isValidExprName :: Name -> Bool
isValidExprName name = case name of
    firstChar : restOfChars | isLower firstChar && all isAlphaNum restOfChars -> True
    _ -> False

isValidDataConstructorName :: Name -> Bool
isValidDataConstructorName = isValidTypeName

isValidVarName :: Name -> Bool
isValidVarName = isValidExprName

goToDef :: DefKey -> AppState -> EventM AppResourceName (Next AppState)
goToDef key = case key of
    TypeDefKey k -> goToTypeDef k
    ExprDefKey k -> goToExprDef k

goToTypeDef :: TypeDefKey -> AppState -> EventM AppResourceName (Next AppState)
goToTypeDef key = modifyLocationHistory $ push $ TypeDefView $ TypeDefViewLocation key $ TypeConstructorSelection Nothing

goToExprDef :: ExprDefKey -> AppState -> EventM AppResourceName (Next AppState)
goToExprDef key = modifyLocationHistory $ push $ ExprDefView $ ExprDefViewLocation key []

goBackInLocationHistory :: AppState -> EventM AppResourceName (Next AppState)
goBackInLocationHistory = modifyLocationHistory goBack

goForwardInLocationHistory :: AppState -> EventM AppResourceName (Next AppState)
goForwardInLocationHistory = modifyLocationHistory goForward

modifyLocationHistory :: (History Location -> History Location) -> AppState -> EventM AppResourceName (Next AppState)
modifyLocationHistory modify appState = liftIO (updateDerivedState $ appState & committedLocations %~ modify) >>= continue

initiateRenameDefinition :: AppState -> EventM AppResourceName (Next AppState)
initiateRenameDefinition appState = continue $ appState & editState .~ Naming initialRenameEditor where
    initialRenameEditor = applyEdit gotoEOL $ editor EditorName (Just 1) $ fromMaybe "" $ getCurrentDefName appState

getCurrentDefName :: AppState -> Maybe Name
getCurrentDefName appState = case getLocation appState of
    TypeDefView loc -> join $ preview (typeDefs . ix (view typeDefKey loc) . present . T.typeConstructor . T.typeConstructorName) appState
    ExprDefView loc -> join $ preview (ix (view exprDefKey loc) . name) (getExprDefs appState)
    _ -> Nothing

cancelEdit :: AppState -> EventM AppResourceName (Next AppState)
cancelEdit appState = continue $ appState & editState .~ NotEditing

commitDefName :: AppState -> String -> (String -> Bool) -> EventM AppResourceName (Next AppState)
commitDefName appState newName isValid = case newName of
    "" -> setCurrentDefName Nothing $ appState & editState .~ NotEditing
    _ | isValid newName -> setCurrentDefName (Just newName) $ appState & editState .~ NotEditing
    _ -> continue appState

setCurrentDefName :: Maybe Name -> AppState -> EventM AppResourceName (Next AppState)
setCurrentDefName newName appState = case getLocation appState of
    DefListView _ -> error "setCurrentDefName is not implemented for DefListView"
    TypeDefView loc -> modifyTypeDef (view typeDefKey loc) (T.typeConstructor . T.typeConstructorName .~ newName) appState
    ExprDefView loc -> modifyExprDef (view exprDefKey loc) (name .~ newName) appState

initiateAddTypeConstructorParam :: AppState -> ParamIndex -> EventM AppResourceName (Next AppState)
initiateAddTypeConstructorParam appState index =
    continue $ appState & editState .~ AddingTypeConstructorParam index (editor EditorName (Just 1) "")

initiateAddDataConstructor :: AppState -> DataConstructorIndex -> EventM AppResourceName (Next AppState)
initiateAddDataConstructor appState index =
    continue $ appState & editState .~ AddingDataConstructor index (editor EditorName (Just 1) "")

commitAddTypeConstructorParam :: AppState -> Int -> TypeDefKey -> Name -> EventM AppResourceName (Next AppState)
commitAddTypeConstructorParam appState typeDefKey index name =
    if isValidVarName name
    then appState
        & editState .~ NotEditing
        & committedLocations . present . _TypeDefView . typeDefViewSelection .~ TypeConstructorSelection (Just index)
        & modifyTypeDef typeDefKey (T.typeConstructor . T.typeConstructorParams %~ insertAt index name)
    else continue appState

commitAddDataConstructor :: AppState -> TypeDefKey -> Int -> Name -> EventM AppResourceName (Next AppState)
commitAddDataConstructor appState typeDefKey index name =
    if isValidDataConstructorName name
    then appState
        & editState .~ NotEditing
        & committedLocations . present . _TypeDefView . typeDefViewSelection .~ DataConstructorSelection index Nothing
        & modifyTypeDef typeDefKey (T.dataConstructors %~ insertAt index (T.DataConstructor name []))
    else continue appState

modifyTypeDefs :: AppState -> (Map.Map TypeDefKey (History TypeDef) -> Map.Map TypeDefKey (History TypeDef)) -> EventM AppResourceName (Next AppState)
modifyTypeDefs appState modify = liftIO (handleTypeDefsChange $ appState & typeDefs %~ modify) >>= continue

modifyExprDefs :: AppState -> (Map.Map ExprDefKey (History ExprDef) -> Map.Map ExprDefKey (History ExprDef)) -> EventM AppResourceName (Next AppState)
modifyExprDefs appState modify = liftIO (handleExprDefsChange $ appState & committedExprDefs %~ modify) >>= continue

modifyTypeDef :: TypeDefKey -> (TypeDef -> TypeDef) -> AppState -> EventM AppResourceName (Next AppState)
modifyTypeDef key modify appState = modifyTypeDefs appState $ ix key %~ History.step modify

modifyExprDef :: ExprDefKey -> (ExprDef -> ExprDef) -> AppState -> EventM AppResourceName (Next AppState)
modifyExprDef key modify appState = modifyExprDefs appState $ ix key %~ History.step modify

printAutocompleteItem :: (ExprDefKey -> Name) -> Selectable -> String
printAutocompleteItem getExprName item = case item of
    Expr (E.Def key) -> getExprName key
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

handleTypeDefsChange :: AppState -> IO AppState
handleTypeDefsChange appState = do
    writeTypeDefs appState
    updateDerivedState appState

handleExprDefsChange :: AppState -> IO AppState
handleExprDefsChange appState = do
    writeExprDefs appState
    updateDerivedState appState

updateDerivedState :: AppState -> IO AppState
updateDerivedState appState = do
    let getTypeDef = getPresentTypeDef $ view typeDefs appState
    let exprs = getExprs appState
    let location = getLocation appState
    maybeDerivedState <- traverse (createDerivedState getTypeDef exprs) (preview _ExprDefView location)
    return $ appState & derivedState .~ maybeDerivedState

updateEvalResult :: AppState -> IO AppState
updateEvalResult appState = do
    let exprs = getExprs appState
    let location = getLocation appState
    maybeEvalResult <- traverse (createEvalResult exprs) (preview _ExprDefView location)
    return $ appState & derivedState . _Just . evalResult %~ flip fromMaybe maybeEvalResult

getExprs :: AppState -> Map.Map ExprDefKey Expr
getExprs appState = view expr <$> getExprDefs appState

getExprDefs :: AppState -> Map.Map ExprDefKey ExprDef
getExprDefs appState = case (appState ^. editState, getLocation appState) of
    (EditingExpr e _ _ _, ExprDefView loc) -> committedDefs & ix (view exprDefKey loc) %~ expr .~ e
    _ -> committedDefs
    where committedDefs = view present <$> appState ^. committedExprDefs

getLocation :: AppState -> Location
getLocation appState = case appState ^. editState of
    AddingDataConstructorParam dataConstructorIndex paramIndex _ ->
        committedLocation & _TypeDefView . typeDefViewSelection .~ DataConstructorSelection dataConstructorIndex (Just paramIndex)
    EditingExpr _ path _ _ -> committedLocation & _ExprDefView . exprDefViewSelection .~ NonEmpty.head path
    _ -> committedLocation
    where committedLocation = appState ^. committedLocations . present

createDerivedState :: (TypeDefKey -> TypeDef) -> Map.Map ExprDefKey Expr -> ExprDefViewLocation -> IO DerivedState
createDerivedState getTypeDef defs location =
    DerivedState (createInferResult getTypeDef defs $ view exprDefKey location) <$> createEvalResult defs location

createInferResult :: (TypeDefKey -> TypeDef) -> Map.Map ExprDefKey Expr -> ExprDefKey -> InferResult
createInferResult getTypeDef defs defKey = Infer.inferType (T.getDataConstructorType getTypeDef) defs $ defs Map.! defKey

getPresentTypeDef :: Map.Map TypeDefKey (History TypeDef) -> TypeDefKey -> TypeDef
getPresentTypeDef typeDefs key = typeDefs Map.! key ^. present

createEvalResult :: Map.Map ExprDefKey Expr -> ExprDefViewLocation -> IO EvalResult
createEvalResult exprs (ExprDefViewLocation defKey selectionPath) = do
    let maybeExpr = Map.lookup defKey exprs
    let maybeSelected = maybeExpr >>= getItemAtPathInExpr selectionPath
    let maybeSelectedExpr = maybeSelected >>= preview _Expr
    let maybeSelectionValue = maybeSelectedExpr >>= eval exprs
    timeoutResult <- timeout 10000 $ evaluate $ force maybeSelectionValue
    return $ case timeoutResult of
        Just (Just v) -> Value v
        Just Nothing -> Error
        Nothing -> Timeout
