module PrettyPrintValue (prettyPrintValue) where

import Util
import qualified Value as V

prettyPrintValue :: (c -> String) -> V.Value c -> Maybe String
prettyPrintValue getName value = case value of
    V.Fn _ -> Nothing
    V.Constructor key childMaybes -> do
        children <- sequence childMaybes
        let printChild child = inParensIf (isMultiWord child) <$> prettyPrintValue getName child
        printedChildren <- traverse printChild children
        return $ unwords $ getName key : printedChildren
    V.Int n -> Just $ show n

isMultiWord :: V.Value c -> Bool
isMultiWord value = case value of
    V.Constructor _ values -> not $ null values
    _ -> False
