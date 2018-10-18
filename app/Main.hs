{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Control.DeepSeq
import Control.Exception.Base (evaluate)
import Control.Lens hiding (DefName)
import Control.Monad.IO.Class
import Data.Char
import Data.List
import Data.Maybe
import Data.Text.Zipper
import GHC.Generics
import System.Timeout
import Text.Read
import Brick hiding (Location)
import Brick.Widgets.Border
import Brick.Widgets.Edit
import Safe
import Eval
import History
import PrettyPrintType
import PrettyPrintValue
import Util
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector as Vec
import qualified Brick.Types
import qualified Brick.Widgets.List as ListWidget
import qualified Graphics.Vty as Vty
import qualified BuiltInPrimitives as BP
import qualified BuiltInTypes as BT
import qualified Defs
import qualified Expr as E
import qualified Infer
import qualified Pattern as P
import qualified Type as T
import qualified Value as V

data AppState = AppState
    { _customTypeDefs :: Map.Map Id (Maybe Name, History TypeDef)
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
data TypeDefKey = BuiltIn BT.TypeDefKey | Custom Id deriving (Eq, Generic, NFData)
type Expr = E.Expr Id DataConstructorKey BP.Primitive
type DataConstructorKey = T.DataConstructorKey TypeDefKey
type Alternative = E.Alternative Id DataConstructorKey BP.Primitive
type Pattern = P.Pattern DataConstructorKey
type Type = T.Type TypeDefKey
type Value = V.Value DataConstructorKey
type InferResult = Infer.InferResult TypeDefKey
type TypeTree = Infer.TypeTree TypeDefKey
type TypeError = Infer.TypeError TypeDefKey
data AppResourceName = DefListName | EditorName | AutocompleteName | Viewport deriving (Eq, Ord, Show)
type AppWidget = Widget AppResourceName
data WrappingStyle = NoParens | OneWordPerLine | Parens deriving (Eq, Enum, Bounded)
data Location = DefListView SelectedDefKey | ExprDefView ExprDefViewLocation
type SelectedDefKey = DefKey
data DefKey = TypeDefKey TypeDefKey | ExprDefKey Id deriving Eq
type ExprDefViewLocation = (Id, E.Path)
data EditState = NotEditing | Naming EditorState | SelectionEditing EditorState (Maybe AutocompleteState)
type EditorState = Editor String AppResourceName
data AutocompleteState = AutocompleteState AutocompleteList EditorExtent
type AutocompleteList = ListWidget.List AppResourceName Selectable
type EditorExtent = Extent AppResourceName
data EvalResult = Timeout | Error | Value Value
data Selectable = Expr Expr | Pattern Pattern
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

instance E.PrimitiveType BP.Primitive TypeDefKey where
    getType = BP.getType BuiltIn
instance E.PrimitiveValue BP.Primitive DataConstructorKey where
    getValue = BP.getValue BuiltIn

main :: IO AppState
main = defaultMain app initialState

initialState :: AppState
initialState = AppState typeDefs defs locationHistory NoParens clipboard NotEditing Nothing where
    typeDefs = Map.empty
    defs = initialDefs
    clipboard = Clipboard Nothing Nothing
    locationHistory = History.create $ DefListView $ TypeDefKey $ BuiltIn minBound

initialDefs :: Map.Map Id (Maybe Name, History Expr)
initialDefs = Map.mapKeys modifyDefKey $ Map.map f Defs.defs where
    f (name, expr) = (Just name, History.create $ replaceKeysInExpr modifyDefKey modifyConstructorKey expr)
    modifyDefKey key = Map.findWithDefault (-1) key defKeyToIdMap
    defKeyToIdMap = Map.fromList $ zip (Map.keys Defs.defs) [0..]
    modifyConstructorKey = over T.typeDefKey BuiltIn

replaceKeysInExpr :: (d1 -> d2) -> (c1 -> c2) -> E.Expr d1 c1 p -> E.Expr d2 c2 p
replaceKeysInExpr modifyDefKey modifyConstructorKey expr = case expr of
    E.Hole -> E.Hole
    E.Def key -> E.Def $ modifyDefKey key
    E.Var v -> E.Var v
    E.Fn alts -> E.Fn $ replaceInAlt <$> alts
    E.Call callee arg -> E.Call (replaceInExpr callee) (replaceInExpr arg)
    E.Constructor key -> E.Constructor $ modifyConstructorKey key
    E.Int n -> E.Int n
    E.Primitive p -> E.Primitive p
    where
        replaceInExpr = replaceKeysInExpr modifyDefKey modifyConstructorKey
        replaceInAlt (pattern, expr) = (fmap modifyConstructorKey pattern, replaceInExpr expr)

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
    ExprDefView defViewLocation -> drawExprDefView appState defViewLocation

drawDefListView :: AppState -> SelectedDefKey -> [AppWidget]
drawDefListView appState selectedDefKey = [ renderTitle "Definitions" <=> renderedList ] where
    renderedList = vBox $ renderItem <$> getDefKeys appState
    renderItem key = grayIf (key /= selectedDefKey) (str $ getDefName appState key)
    grayIf cond = if cond then modifyDefAttr $ flip Vty.withForeColor gray else id

getDefName :: AppState -> DefKey -> Name
getDefName appState key = case key of
    TypeDefKey k -> getTypeName appState k
    ExprDefKey id -> getExprName appState id

getTypeName :: AppState -> TypeDefKey -> Name
getTypeName appState key = case key of
    BuiltIn k -> show k
    Custom id -> fromMaybe "Unnamed" $ fst $ view customTypeDefs appState Map.! id

getExprName :: AppState -> Id -> Name
getExprName appState id = fromMaybe "unnamed" $ fst $ view exprDefs appState Map.! id

gray :: Vty.Color
gray = Vty.rgbColor 128 128 128 -- this shade seems ok on both light and dark backgrounds

drawExprDefView :: AppState -> ExprDefViewLocation -> [AppWidget]
drawExprDefView appState (defId, selectionPath) = ui where
    AppState customTypeDefs defs _ wrappingStyle _ editState (Just (DerivedState inferResult evalResult)) = appState
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
    (maybeDefName, defHistory) = defs Map.! defId
    coloredExpr = modifyDefAttr (flip Vty.withForeColor gray) renderedExpr -- unselected parts of the expression are gray
    expr = view present defHistory
    (_, renderedExpr) = renderWithAttrs wrappingStyle editState (ContainsSelection selectionPath) maybeTypeError (renderExpr defNames expr)
    defNames = Map.mapMaybe fst defs
    maybeTypeError = preview Infer._TypeError inferResult
    maybeSelectionType = getTypeAtPathInInferResult selectionPath inferResult
    bottomStr = case maybeSelectionType of
        Just t -> evalStr ++ ": " ++ prettyPrintType (getTypeName appState) t
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
    SelectionEditing editor _ | selected -> (OneWord, visible . highlight $ hLimit (length editStr + 1) renderedEditor) where
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

renderExpr :: Map.Map Id Name -> Expr -> Renderer
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
    E.Constructor key -> (OneWord, str $ view T.constructorName key)
    E.Int n -> (OneWord, str $ show n)
    E.Primitive p -> (OneWord, str $ E.getDisplayName p)

withParensIf :: Bool -> Widget n -> Widget n
withParensIf cond w = if cond then str "(" <+> w <+> str ")" else w

renderAlternative :: Map.Map Id Name -> RenderChild -> Int -> Alternative -> RenderResult
renderAlternative defNames (RenderChild renderChild) alternativeIndex (pattern, expr) =
    if exprResultType == MultiLine
    then (MultiLine, renderedPattern <+> str " ->" <=> renderedExpr)
    else (OneLine, renderedPattern <+> str " -> " <+> renderedExpr)
    where
        (_, renderedPattern) = renderChild (2 * alternativeIndex) $ renderPattern pattern
        (exprResultType, renderedExpr) = renderChild (2 * alternativeIndex + 1) $ renderExpr defNames expr

renderPattern :: Pattern -> Renderer
renderPattern pattern wrappingStyle (RenderChild renderChild) = case pattern of
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

getChildSelection :: Selection -> E.ChildIndex -> Selection
getChildSelection selection index = case selection of
    ContainsSelection (i:childPath) -> if i == index then ContainsSelection childPath else NoSelection
    ContainsSelection [] -> WithinSelection
    WithinSelection -> WithinSelection
    NoSelection -> NoSelection

getChildTypeError :: Maybe TypeError -> E.ChildIndex -> Maybe TypeError
getChildTypeError maybeTypeError index = childResult >>= preview Infer._TypeError
    where childResult = (!! index) <$> maybeTypeError

getTypeAtPathInInferResult :: E.Path -> InferResult -> Maybe Type
getTypeAtPathInInferResult path inferResult = case inferResult of
    Infer.Typed typeTree -> getTypeAtPathInTypeTree path typeTree
    Infer.TypeError childResults -> case path of
        [] -> Nothing
        index:restOfPath -> getTypeAtPathInInferResult restOfPath $ childResults !! index

getTypeAtPathInTypeTree :: E.Path -> TypeTree -> Maybe Type
getTypeAtPathInTypeTree path (Infer.TypeTree t children) = case path of
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

getChildInPattern :: Pattern -> E.ChildIndex -> Maybe Selectable
getChildInPattern pattern index = case pattern of
    P.Constructor _ patterns -> Pattern <$> getItemAtIndex index patterns
    _ -> Nothing

handleEvent :: AppState -> BrickEvent n e -> EventM AppResourceName (Next AppState)
handleEvent appState (VtyEvent event) = case view present $ view locationHistory appState of
    DefListView selectedDefKey -> handleEventOnDefListView appState event selectedDefKey
    ExprDefView location -> handleEventOnExprDefView appState event location

handleEventOnDefListView :: AppState -> Vty.Event -> DefKey -> EventM AppResourceName (Next AppState)
handleEventOnDefListView appState event selectedDefKey = case event of
    Vty.EvKey Vty.KEnter [] -> goToDef selectedDefKey appState
    Vty.EvKey Vty.KUp [] -> select $ fromMaybe selectedDefKey maybePrevDefKey
    Vty.EvKey Vty.KDown [] -> select $ fromMaybe selectedDefKey maybeNextDefKey
    Vty.EvKey (Vty.KChar 'g') [] -> goBackInLocationHistory appState
    Vty.EvKey (Vty.KChar 'G') [] -> goForwardInLocationHistory appState
    Vty.EvKey (Vty.KChar 'n') [] -> addNewExprDef
    Vty.EvKey (Vty.KChar 'q') [] -> halt appState
    _ -> continue appState
    where
        select defKey = continue $ appState & locationHistory . present . _DefListView .~ defKey
        maybePrevDefKey = fmap (subtract 1) maybeSelectedIndex >>= flip getItemAtIndex defKeys
        maybeNextDefKey = fmap (+ 1) maybeSelectedIndex >>= flip getItemAtIndex defKeys
        maybeSelectedIndex = elemIndex selectedDefKey defKeys
        defKeys = getDefKeys appState
        addNewExprDef = liftIO createNewAppState >>= continue where
            createNewAppState = updateDerivedState $ appState
                & exprDefs %~ Map.insert newDefId (Nothing, History.create E.Hole)
                & locationHistory %~ (push (ExprDefView (newDefId, []))) . (present .~ DefListView (ExprDefKey newDefId))
            newDefId = fst (Map.findMax $ view exprDefs appState) + 1

getDefKeys :: AppState -> [DefKey]
getDefKeys appState = (TypeDefKey <$> getTypeDefKeys appState) ++ (ExprDefKey <$> getExprDefIds appState)

getTypeDefKeys :: AppState -> [TypeDefKey]
getTypeDefKeys appState = (BuiltIn <$> BT.typeDefKeys) ++ (Custom <$> getCustomTypeDefIds appState)

getCustomTypeDefIds :: AppState -> [Id]
getCustomTypeDefIds = Map.keys . view customTypeDefs

getExprDefIds :: AppState -> [Id]
getExprDefIds = Map.keys . view exprDefs

handleEventOnExprDefView :: AppState -> Vty.Event -> ExprDefViewLocation -> EventM AppResourceName (Next AppState)
handleEventOnExprDefView appState event (defId, selectionPath) = case currentEditState of
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
            let isMatch = isSimilarToEditorContent . printAutocompleteItem exprDefNames
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
        currentCustomTypeDefs = view customTypeDefs appState
        currentExprDefs = view exprDefs appState
        exprDefIds = Map.keys currentExprDefs
        exprDefNames = Map.mapMaybe fst currentExprDefs
        (maybeDefName, defHistory) = currentExprDefs Map.! defId
        defExpr = view present defHistory
        currentClipboard = view clipboard appState
        currentEditState = view editState appState
        initiateRename = setEditState $ Naming initialRenameEditor
        initialRenameEditor = applyEdit gotoEOL $ editor EditorName (Just 1) $ fromMaybe "" maybeDefName
        commitName newName = continue $ appState
            & exprDefs . ix defId . _1 .~ (if null newName then Nothing else Just newName)
            & editState .~ NotEditing
        navToParent = nav parentPath
        navToChild = nav pathToFirstChildOfSelected
        navBackward = nav prevSiblingPath
        navForward = nav nextSiblingPath
        nav path = case getItemAtPath path of
            Just itemAtPath -> liftIO getNewAppState >>= continue where
                getNewAppState = updateEvalResult $ appState & locationHistory . present . _ExprDefView . _2 .~ path
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
        selected = fromJustNote "selection path should be a valid path" $ getItemAtPath selectionPath
        getItemAtPath path = getItemAtPathInExpr path defExpr
        goToDefinition = case selected of
            Expr (E.Def defId) | Map.member defId currentExprDefs -> goToExprDef defId appState
            _ -> continue appState
        initiateSelectionEdit = setEditState $ SelectionEditing initialSelectionEditor Nothing
        initialSelectionEditor = applyEdit gotoEOL $ editor EditorName (Just 1) initialSelectionEditorContent
        initialSelectionEditorContent = printAutocompleteItem exprDefNames selected
        cancelEdit = setEditState NotEditing
        commitEditorContent editorContent = modifyDef $ case readMaybe editorContent of
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
                & exprDefs . ix defId . _2 %~ push newExpr
                & locationHistory . present . _ExprDefView . _2 .~ pathToExprContainingSelection
                & editState .~ NotEditing
            newExpr = modifyAtPathInExpr pathToExprContainingSelection modify id defExpr
            pathToExprContainingSelection = dropPatternPartOfPath defExpr selectionPath
        addAlternativeToSelected = maybe (continue appState) modifyDef (addAlternativeAtPath selectionPath defExpr)
        deleteSelected = modifyDef $ replaceSelected E.Hole P.Wildcard
        modifyDef newExpr = liftIO createNewAppState >>= continue where
            createNewAppState = updateDerivedState $ appState
                & exprDefs . ix defId . _2 %~ push newExpr
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
        undo = liftIO (updateDerivedState $ appState & exprDefs . ix defId . _2 %~ goBack) >>= continue
        redo = liftIO (updateDerivedState $ appState & exprDefs . ix defId . _2 %~ goForward) >>= continue

goToDef :: DefKey -> AppState -> EventM AppResourceName (Next AppState)
goToDef key = case key of
    TypeDefKey k -> continue
    ExprDefKey id -> goToExprDef id

goToExprDef :: Id -> AppState -> EventM AppResourceName (Next AppState)
goToExprDef id = modifyLocationHistory (push $ ExprDefView (id, []))

goBackInLocationHistory :: AppState -> EventM AppResourceName (Next AppState)
goBackInLocationHistory = modifyLocationHistory goBack

goForwardInLocationHistory :: AppState -> EventM AppResourceName (Next AppState)
goForwardInLocationHistory = modifyLocationHistory goForward

modifyLocationHistory :: (History Location -> History Location) -> AppState -> EventM AppResourceName (Next AppState)
modifyLocationHistory modify appState = liftIO (updateDerivedState $ appState & locationHistory %~ modify) >>= continue

printAutocompleteItem :: Map.Map Id Name -> Selectable -> String
printAutocompleteItem defNames item = case item of
    Expr (E.Def id) -> fromMaybe "unnamed" $ Map.lookup id defNames
    Expr (E.Var name) -> name
    Expr (E.Constructor key) -> view T.constructorName key
    Expr (E.Int n) -> show n
    Expr (E.Primitive p) -> E.getDisplayName p
    Pattern (P.Var name) -> name
    Pattern (P.Constructor key _) -> view T.constructorName key
    Pattern (P.Int n) -> show n
    _ -> ""

getVarsAtPath :: E.Path -> E.Expr d c p -> [E.VarName]
getVarsAtPath path expr = case path of
    [] -> []
    edge:restOfPath -> case expr of
        E.Fn alts | odd edge -> getVars pattern ++ getVarsAtPath restOfPath body where
            (pattern, body) = alts NonEmpty.!! div edge 2
        E.Call callee _ | edge == 0 -> getVarsAtPath restOfPath callee
        E.Call _ arg | edge == 1 -> getVarsAtPath restOfPath arg
        _ -> error "invalid path"

getVars :: P.Pattern c -> [E.VarName]
getVars (P.Var name) = [name]
getVars (P.Constructor _ children) = children >>= getVars
getVars _ = []

modifyAtPathInExpr :: E.Path -> (E.Expr d c p -> E.Expr d c p) -> (P.Pattern c -> P.Pattern c) -> E.Expr d c p -> E.Expr d c p
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

modifyAtPathInPattern :: E.Path -> (P.Pattern t -> P.Pattern t) -> P.Pattern t -> P.Pattern t
modifyAtPathInPattern path modify pattern = case path of
    [] -> modify pattern
    edge:restOfPath -> case pattern of
        P.Constructor name children -> P.Constructor name $ modifyItemAtIndex edge (modifyAtPathInPattern restOfPath modify) children
        _ -> error "invalid path"

dropPatternPartOfPath :: E.Expr d c p -> E.Path -> E.Path
dropPatternPartOfPath expr path = case path of
    [] -> []
    edge:restOfPath -> case expr of
        E.Fn alts -> if even edge then [] else edge : dropPatternPartOfPath (snd $ alts NonEmpty.!! div edge 2) restOfPath
        E.Call callee arg | edge == 0 -> 0 : dropPatternPartOfPath callee restOfPath
        E.Call callee arg | edge == 1 -> 1 : dropPatternPartOfPath arg restOfPath
        _ -> error "invalid path"

addAlternativeAtPath :: E.Path -> E.Expr d c p -> Maybe (E.Expr d c p)
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

updateDerivedState :: AppState -> IO AppState
updateDerivedState appState = do
    let getCurrentTypeDef = getTypeDef $ view customTypeDefs appState
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
getTypeDef customTypeDefs key = case key of
    BuiltIn k -> BT.getTypeDef k & T.dataConstructors %~ fmap (fmap BuiltIn)
    Custom id -> view present . snd $ customTypeDefs Map.! id

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
