{-# LANGUAGE LambdaCase #-}

module Primitive where

import qualified Type as T
import qualified Value as V

data Primitive
    = Plus
    | Minus
    | Times
    | Signum
    deriving (Eq, Read, Show, Bounded, Enum)

getDisplayName :: Primitive -> String
getDisplayName p = case p of
    Plus -> "+"
    Minus -> "-"
    Times -> "*"
    Signum -> "signum"

getType :: Primitive -> T.Type v d
getType p = case p of
    Plus -> binaryIntegerOpType T.Integer
    Minus -> binaryIntegerOpType T.Integer
    Times -> binaryIntegerOpType T.Integer
    Signum -> T.fn T.Integer T.Integer

getValue :: Primitive -> V.Value c
getValue p = case p of
    Plus -> binaryIntegerOpValue $ \a b -> V.Integer (a + b)
    Minus -> binaryIntegerOpValue $ \a b -> V.Integer (a - b)
    Times -> binaryIntegerOpValue $ \a b -> V.Integer (a * b)
    Signum -> V.Fn $ \case
        Just (V.Integer a) -> Just $ V.Integer $ signum a
        _ -> Nothing

binaryIntegerOpType :: T.Type v d -> T.Type v d
binaryIntegerOpType resultType = T.fn T.Integer $ T.fn T.Integer resultType

binaryIntegerOpValue :: (Integer -> Integer -> V.Value t) -> V.Value t
binaryIntegerOpValue f = V.Fn $ \maybeAVal -> Just $ V.Fn $ \maybeBVal -> case (maybeAVal, maybeBVal) of
    -- once both arguments were passed to the operation, it's time to evaluate them
    (Just (V.Integer a), Just (V.Integer b)) -> Just $ f a b
    _ -> Nothing
