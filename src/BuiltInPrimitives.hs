{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module BuiltInPrimitives where

import qualified BuiltInTypes
import qualified Expr as E
import qualified Type as T
import qualified Value as V

data Primitive
    = Equals
    | Plus
    | Minus
    | Times
    deriving (Eq, Show, Bounded, Enum)

getDisplayName :: Primitive -> String
getDisplayName p = case p of
    Equals -> "="
    Plus -> "+"
    Minus -> "-"
    Times -> "*"

getType :: (BuiltInTypes.TypeDefKey -> t) -> Primitive -> T.Type t
getType modifyTypeDefKey p = case p of
    Equals -> binaryIntOpType $ T.Constructor (modifyTypeDefKey BuiltInTypes.Bool) []
    Plus -> binaryIntOpType T.Int
    Minus -> binaryIntOpType T.Int
    Times -> binaryIntOpType T.Int

getValue :: (BuiltInTypes.TypeDefKey -> t) -> Primitive -> V.Value (T.DataConstructorKey t)
getValue modifyTypeDefKey p = case p of
    Equals -> binaryIntOpValue $ \a b ->
        V.Constructor (T.DataConstructorKey (modifyTypeDefKey BuiltInTypes.Bool) $ if a == b then "True" else "False") []
    Plus -> binaryIntOpValue $ \a b -> V.Int (a + b)
    Minus -> binaryIntOpValue $ \a b -> V.Int (a - b)
    Times -> binaryIntOpValue $ \a b -> V.Int (a * b)

binaryIntOpType :: T.Type t -> T.Type t
binaryIntOpType resultType = T.Fn T.Int $ T.Fn T.Int resultType

binaryIntOpValue :: (Int -> Int -> V.Value t) -> V.Value t
binaryIntOpValue f = V.Fn $ \maybeAVal -> Just $ V.Fn $ \maybeBVal -> case (maybeAVal, maybeBVal) of
    -- once both arguments were passed to the operation, it's time to evaluate them
    (Just (V.Int a), Just (V.Int b)) -> Just $ f a b
    _ -> Nothing

instance E.PrimitiveName Primitive where
    getDisplayName = getDisplayName
instance E.PrimitiveType Primitive BuiltInTypes.TypeDefKey where
    getType = getType id
instance E.PrimitiveValue Primitive BuiltInTypes.DataConstructorKey where
    getValue = getValue id
