module Primitive where

import qualified Type as T
import qualified Value as V

data Primitive
    = Equals
    | Plus
    | Minus
    | Times
    deriving (Eq, Show, Bounded, Enum)

getDisplayName :: Primitive -> String
getDisplayName Equals = "="
getDisplayName Plus = "+"
getDisplayName Minus = "-"
getDisplayName Times = "*"

getType :: Primitive -> T.Type
getType Equals = binaryIntOpType $ T.Constructor "Bool" []
getType Plus = binaryIntOpType T.Int
getType Minus = binaryIntOpType T.Int
getType Times = binaryIntOpType T.Int

getValue :: Primitive -> V.Value
getValue Equals = binaryIntOpValue $ \a b -> V.Constructor (if a == b then "True" else "False") []
getValue Plus = binaryIntOpValue $ \a b -> V.Int (a + b)
getValue Minus = binaryIntOpValue $ \a b -> V.Int (a - b)
getValue Times = binaryIntOpValue $ \a b -> V.Int (a * b)

binaryIntOpType :: T.Type -> T.Type
binaryIntOpType resultType = T.Fn T.Int $ T.Fn T.Int resultType

binaryIntOpValue :: (Int -> Int -> V.Value) -> V.Value
binaryIntOpValue f = V.Fn $ \maybeAVal -> Just $ V.Fn $ \maybeBVal -> case (maybeAVal, maybeBVal) of
    -- once both arguments were passed to the operation, it's time to evaluate them
    (Just (V.Int a), Just (V.Int b)) -> Just $ f a b
    _ -> Nothing
