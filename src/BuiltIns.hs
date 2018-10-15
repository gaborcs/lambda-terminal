module BuiltIns where

import qualified Expr as E
import qualified Type as T
import qualified Value as V

data Primitive
    = Equals
    | Plus
    | Minus
    | Times
    deriving (Eq, Show, Bounded, Enum)

instance E.Primitive Primitive where
    getDisplayName p = case p of
        Equals -> "="
        Plus -> "+"
        Minus -> "-"
        Times -> "*"
    getType p = case p of
        Equals -> binaryIntOpType $ T.Constructor "Bool" []
        Plus -> binaryIntOpType T.Int
        Minus -> binaryIntOpType T.Int
        Times -> binaryIntOpType T.Int
    getValue p = case p of
        Equals -> binaryIntOpValue $ \a b -> V.Constructor (if a == b then "True" else "False") []
        Plus -> binaryIntOpValue $ \a b -> V.Int (a + b)
        Minus -> binaryIntOpValue $ \a b -> V.Int (a - b)
        Times -> binaryIntOpValue $ \a b -> V.Int (a * b)

binaryIntOpType :: T.Type -> T.Type
binaryIntOpType resultType = T.Fn T.Int $ T.Fn T.Int resultType

binaryIntOpValue :: (Int -> Int -> V.Value) -> V.Value
binaryIntOpValue f = V.Fn $ \maybeAVal -> Just $ V.Fn $ \maybeBVal -> case (maybeAVal, maybeBVal) of
    -- once both arguments were passed to the operation, it's time to evaluate them
    (Just (V.Int a), Just (V.Int b)) -> Just $ f a b
    _ -> Nothing
