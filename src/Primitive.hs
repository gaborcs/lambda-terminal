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
    Plus -> binaryIntOpType T.Int
    Minus -> binaryIntOpType T.Int
    Times -> binaryIntOpType T.Int
    Signum -> T.fn T.Int T.Int

getValue :: Primitive -> V.Value c
getValue p = case p of
    Plus -> binaryIntOpValue $ \a b -> V.Int (a + b)
    Minus -> binaryIntOpValue $ \a b -> V.Int (a - b)
    Times -> binaryIntOpValue $ \a b -> V.Int (a * b)
    Signum -> V.Fn $ \case
        Just (V.Int a) -> Just $ V.Int $ signum a
        _ -> Nothing

binaryIntOpType :: T.Type v d -> T.Type v d
binaryIntOpType resultType = T.fn T.Int $ T.fn T.Int resultType

binaryIntOpValue :: (Int -> Int -> V.Value t) -> V.Value t
binaryIntOpValue f = V.Fn $ \maybeAVal -> Just $ V.Fn $ \maybeBVal -> case (maybeAVal, maybeBVal) of
    -- once both arguments were passed to the operation, it's time to evaluate them
    (Just (V.Int a), Just (V.Int b)) -> Just $ f a b
    _ -> Nothing
