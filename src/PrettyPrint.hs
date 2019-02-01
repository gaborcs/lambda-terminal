module PrettyPrint where

import qualified Value as V

prettyPrintValue :: (c -> String) -> V.Value c -> Maybe String
prettyPrintValue getName value = case value of
    V.Fn _ -> Nothing
    V.Constructor key childMaybes -> do
        children <- sequence childMaybes
        let printChild child = inParensIf (isMultiWord child) <$> prettyPrintValue getName child
        printedChildren <- traverse printChild children
        return $ unwords $ getName key : printedChildren
    V.Integer n -> Just $ show n
    V.String s -> Just $ show s

inParensIf :: Bool -> String -> String
inParensIf cond s = if cond then "(" ++ s ++ ")" else s

isMultiWord :: V.Value c -> Bool
isMultiWord value = case value of
    V.Constructor _ values -> not $ null values
    _ -> False
