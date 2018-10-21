{-# LANGUAGE FlexibleInstances #-}

module Primitive where

import qualified BuiltInTypes
import qualified Type as T
import qualified Value as V

data Primitive
    = Equals
    | Plus
    | Minus
    | Times
    deriving (Eq, Read, Show, Bounded, Enum)

getDisplayName :: Primitive -> String
getDisplayName p = case p of
    Equals -> "="
    Plus -> "+"
    Minus -> "-"
    Times -> "*"

getPrimitiveType :: Primitive -> T.Type BuiltInTypes.TypeDefKey
getPrimitiveType p = case p of
    Equals -> binaryIntOpType $ T.Constructor BuiltInTypes.Bool []
    Plus -> binaryIntOpType T.Int
    Minus -> binaryIntOpType T.Int
    Times -> binaryIntOpType T.Int

getPrimitiveValue :: (BuiltInTypes.TypeDefKey -> t) -> Primitive -> V.Value (T.DataConstructorKey t)
getPrimitiveValue modifyTypeDefKey p = case p of
    Equals -> binaryIntOpValue $ \a b ->
        V.Constructor (T.DataConstructorKey (modifyTypeDefKey BuiltInTypes.Bool) $ if a == b then "True" else "False") []
    Plus -> binaryIntOpValue $ \a b -> V.Int (a + b)
    Minus -> binaryIntOpValue $ \a b -> V.Int (a - b)
    Times -> binaryIntOpValue $ \a b -> V.Int (a * b)

class PrimitiveType t where
    getType :: Primitive -> T.Type t
class PrimitiveValue c where
    getValue :: Primitive -> V.Value c

instance PrimitiveType BuiltInTypes.TypeDefKey where
    getType = getPrimitiveType
instance PrimitiveValue BuiltInTypes.DataConstructorKey where
    getValue = getPrimitiveValue id

binaryIntOpType :: T.Type t -> T.Type t
binaryIntOpType resultType = T.Fn T.Int $ T.Fn T.Int resultType

binaryIntOpValue :: (Int -> Int -> V.Value t) -> V.Value t
binaryIntOpValue f = V.Fn $ \maybeAVal -> Just $ V.Fn $ \maybeBVal -> case (maybeAVal, maybeBVal) of
    -- once both arguments were passed to the operation, it's time to evaluate them
    (Just (V.Int a), Just (V.Int b)) -> Just $ f a b
    _ -> Nothing
