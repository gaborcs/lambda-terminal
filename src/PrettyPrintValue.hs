module PrettyPrintValue where

import qualified Value as V

prettyPrintValue :: V.Value -> String
prettyPrintValue value = case value of
    V.Fn _ -> "fn"
    V.Constructor name values -> unwords $ name : fmap (withParensIf isComplex) values
    V.Int n -> show n

withParensIf :: (V.Value -> Bool) -> V.Value -> String
withParensIf pred v = if pred v then "(" ++ s ++ ")" else s where
    s = prettyPrintValue v

isComplex :: V.Value -> Bool
isComplex value = case value of
    V.Constructor _ values -> not $ null values
    _ -> False
