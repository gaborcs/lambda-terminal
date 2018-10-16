module PrettyPrintValue (prettyPrintValue) where

import qualified Value as V

prettyPrintValue :: (c -> String) -> V.Value c -> Maybe String
prettyPrintValue getName value = case value of
    V.Fn _ -> Nothing
    V.Constructor key values -> do
        printedValues <- traverse (>>= withParensIf isComplex) values
        return $ unwords $ getName key : printedValues
    V.Int n -> Just $ show n
    where
        withParensIf pred v = do
            s <- prettyPrintValue getName v
            return $ if pred v then "(" ++ s ++ ")" else s

isComplex :: V.Value c -> Bool
isComplex value = case value of
    V.Constructor _ values -> not $ null values
    _ -> False
