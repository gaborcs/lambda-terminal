module Main where

import Control.DeepSeq
import Control.Exception.Base (evaluate)
import Control.Monad.IO.Class
import Data.List
import Data.Maybe
import System.Timeout
import Brick hiding (Location)
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

data AppState = AppState LocationHistory InferResult EvalResult
type LocationHistory = NonEmpty.NonEmpty Location -- from current to least recent
type Location = (E.ExprName, E.Path)
data EvalResult = Timeout | Error | Value V.Value
data Selectable = Expr E.Expr | Alternative E.Alternative | Pattern P.Pattern
newtype RenderChild n = RenderChild (E.ChildIndex -> Renderer n -> RenderResult n)
type Renderer n = RenderChild n -> RenderResult n
type RenderResult n = (RenderResultType, Widget n)
data RenderResultType = OneWord | OneLine | MultiLine deriving Eq

main :: IO ()
main = do
    let initialLocation = ("main", [])
    let initialLocationHistory = initialLocation NonEmpty.:| []
    initialState <- createAppState initialLocationHistory
    defaultMain app initialState
    return ()

createAppState :: LocationHistory -> IO AppState
createAppState locationHistory = do
    let (exprName, selectionPath) = NonEmpty.head locationHistory
    let expr = fromJust $ Map.lookup exprName defs
    let inferResult = inferType constructorTypes defs expr
    let selected = fromJust $ getItemAtPathInExpr selectionPath expr
    evalResult <- createEvalResult selected
    return $ AppState locationHistory inferResult evalResult

createEvalResult :: Selectable -> IO EvalResult
createEvalResult selectable = do
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
draw (AppState locationHistory inferResult evalResult) = [ padBottom Max renderedExpr <=> str bottomStr ] where
    (exprName, selectionPath) = NonEmpty.head locationHistory
    expr = fromJust $ Map.lookup exprName defs
    (_, renderedExpr) = renderWithAttrs (Just selectionPath) maybeTypeError (renderExpr expr)
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

renderWithAttrs :: Maybe E.Path -> Maybe TypeError -> Renderer n -> RenderResult n
renderWithAttrs maybeSelectionPath maybeTypeError renderer = (renderResultType, highlightIfSelected $ makeRedIfHasError widget) where
    highlightIfSelected = if selected then highlight else id
    selected = maybeSelectionPath == Just []
    makeRedIfHasError = if hasError then makeRed else id
    hasError = maybe False hasErrorAtRoot maybeTypeError
    makeRed = modifyDefAttr $ flip withForeColor red
    (renderResultType, widget) = renderer $ RenderChild renderChild
    renderChild index = renderWithAttrs (getChildPath maybeSelectionPath index) (getChildTypeError maybeTypeError index)

renderExpr :: E.Expr -> Renderer n
renderExpr expr (RenderChild renderChild) = case expr of
    E.Ref exprName -> (OneWord, str exprName)
    E.Var var -> (OneWord, str var)
    E.Fn alternatives -> (MultiLine, str "λ" <+> vBox renderedAlternatives) where
        renderedAlternatives = fmap snd $ zipWith renderChild [0..] $ renderAlternative <$> NonEmpty.toList alternatives
    E.Call callee arg ->
        if calleeResultType == MultiLine || argResultType /= OneWord
        then (MultiLine, renderedCallee <=> indent renderedArg)
        else (OneLine, renderedCallee <+> str " " <+> renderedArg)
        where
            (calleeResultType, renderedCallee) = renderChild 0 (renderExpr callee)
            (argResultType, renderedArg) = renderChild 1 (renderExpr arg)
    E.Constructor name -> (OneWord, str name)
    E.Int n -> (OneWord, str $ show n)
    E.Equals -> (OneWord, str "=")
    E.Plus -> (OneWord, str "+")
    E.Minus -> (OneWord, str "-")
    E.Times -> (OneWord, str "*")

renderAlternative :: E.Alternative -> Renderer n
renderAlternative (pattern, expr) (RenderChild renderChild) = (MultiLine, renderedPattern <=> indent renderedExpr) where
    (_, renderedPattern) = renderChild 0 $ renderPattern pattern
    (_, renderedExpr) = renderChild 1 $ renderExpr expr

renderPattern :: P.Pattern -> Renderer n
renderPattern pattern (RenderChild renderChild) = case pattern of
    P.Var var -> (OneWord, str var)
    P.Constructor name patterns -> (OneLine, hBox $ intersperse (str " ") (str name : renderedChildren)) where
        renderedChildren = fmap snd $ zipWith renderChild [0..] renderers
        renderers = renderPattern <$> patterns

indent :: Widget n -> Widget n
indent w = str "  " <+> w

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

handleEvent :: AppState -> BrickEvent n e -> EventM n (Next AppState)
handleEvent appState event = case event of
    VtyEvent (EvKey KUp []) -> nav prevPath
    VtyEvent (EvKey KDown []) -> nav nextPath
    VtyEvent (EvKey KLeft []) -> nav parentPath
    VtyEvent (EvKey KRight []) -> nav pathToFirstChildOfSelected
    VtyEvent (EvKey KEnter []) -> goToDefinition
    VtyEvent (EvKey KEsc []) -> goBack
    VtyEvent (EvKey (KChar 'q') []) -> halt appState
    _ -> continue appState
    where
        AppState locationHistory inferResult maybeEvalResult = appState
        (exprName, selectionPath) NonEmpty.:| past = locationHistory
        expr = fromJust $ Map.lookup exprName defs
        nav path = case getExprAtPath path of
            Just exprAtPath -> liftIO getNewAppState >>= continue where
                getNewAppState = AppState newLocationHistory inferResult <$> getNewEvalResult
                newLocationHistory = (exprName, path) NonEmpty.:| past
                getNewEvalResult = createEvalResult exprAtPath
            Nothing -> continue appState
        parentPath = if null selectionPath then [] else init selectionPath
        nextPath = if null selectionPath then [] else init selectionPath ++ [last selectionPath + 1]
        prevPath = if null selectionPath then [] else init selectionPath ++ [last selectionPath - 1]
        selectedExpr = getExprAtPath selectionPath
        pathToFirstChildOfSelected = selectionPath ++ [0]
        getExprAtPath path = getItemAtPathInExpr path expr
        goToDefinition = case selectedExpr of
            Just (Expr (E.Ref exprName)) ->
                if Map.member exprName defs
                then liftIO (createAppState $ NonEmpty.cons (exprName, []) locationHistory) >>= continue
                else continue appState
            _ -> continue appState
        goBack = liftIO (createAppState $ fromMaybe locationHistory $ NonEmpty.nonEmpty past) >>= continue
