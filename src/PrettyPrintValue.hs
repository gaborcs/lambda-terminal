module PrettyPrintValue (prettyPrintValue) where

import qualified Value as V

prettyPrintValue :: V.Value -> Maybe String
prettyPrintValue value = case value of
    V.Fn _ -> Nothing
    V.Constructor name values -> do
        printedValues <- traverse (>>= withParensIf isComplex) values
        return $ unwords $ name : printedValues
    V.Int n -> Just $ show n

withParensIf :: (V.Value -> Bool) -> V.Value -> Maybe String
withParensIf pred v = do
    s <- prettyPrintValue v
    return $ if pred v then "(" ++ s ++ ")" else s

isComplex :: V.Value -> Bool
isComplex value = case value of
    V.Constructor _ values -> not $ null values
    _ -> False
