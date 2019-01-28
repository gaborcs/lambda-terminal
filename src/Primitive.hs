{-# LANGUAGE LambdaCase #-}

module Primitive where

import qualified Type as T
import qualified Value as V

data Primitive
    = Plus
    | Minus
    | Times
    | Signum
    | Concat
    deriving (Eq, Read, Show, Bounded, Enum)

getDisplayName :: Primitive -> String
getDisplayName p = case p of
    Plus -> "+"
    Minus -> "-"
    Times -> "*"
    Signum -> "signum"
    Concat -> "concat"

getType :: Primitive -> T.Type v d
getType p = case p of
    Plus -> binaryIntegerOpType T.Integer
    Minus -> binaryIntegerOpType T.Integer
    Times -> binaryIntegerOpType T.Integer
    Signum -> T.fn T.Integer T.Integer
    Concat -> T.fn T.String (T.fn T.String T.String)

getValue :: Primitive -> V.Value c
getValue p = case p of
    Plus -> binaryIntegerOpValue $ \a b -> V.Integer (a + b)
    Minus -> binaryIntegerOpValue $ \a b -> V.Integer (a - b)
    Times -> binaryIntegerOpValue $ \a b -> V.Integer (a * b)
    Signum -> V.Fn $ \case
        Just (V.Integer a) -> Just $ V.Integer $ signum a
        _ -> Nothing
    Concat -> twoParamFnVal $ \case
        (Just (V.String s1), Just (V.String s2)) -> Just $ V.String $ s1 ++ s2
        _ -> Nothing

binaryIntegerOpType :: T.Type v d -> T.Type v d
binaryIntegerOpType resultType = T.fn T.Integer $ T.fn T.Integer resultType

binaryIntegerOpValue :: (Integer -> Integer -> V.Value c) -> V.Value c
binaryIntegerOpValue f = twoParamFnVal $ \case
    (Just (V.Integer a), Just (V.Integer b)) -> Just $ f a b
    _ -> Nothing

-- only evaluates args if and when f does
twoParamFnVal :: ((Maybe (V.Value c), Maybe (V.Value c)) -> Maybe (V.Value c)) -> V.Value c
twoParamFnVal f = V.Fn $ \maybeVal1 -> Just $ V.Fn $ \maybeVal2 -> f (maybeVal1, maybeVal2)
