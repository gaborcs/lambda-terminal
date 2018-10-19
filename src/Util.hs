module Util where

import qualified Data.List.NonEmpty as NonEmpty

getItemAtIndex :: Int -> [a] -> Maybe a
getItemAtIndex index xs = case xs of
    [] -> Nothing
    x:xs' -> if index == 0 then Just x else getItemAtIndex (index - 1) xs'

modifyItemAtIndex :: Int -> (a -> a) -> [a] -> [a]
modifyItemAtIndex index f xs = case xs of
    [] -> []
    x:xs' -> if index == 0 then f x : xs' else x : modifyItemAtIndex (index - 1) f xs'

modifyItemAtIndexInNonEmpty :: Int -> (a -> a) -> NonEmpty.NonEmpty a -> NonEmpty.NonEmpty a
modifyItemAtIndexInNonEmpty index f (x NonEmpty.:| xs) =
    if index == 0 then f x NonEmpty.:| xs else x NonEmpty.:| modifyItemAtIndex (index - 1) f xs

inParensIf :: Bool -> String -> String
inParensIf cond s = if cond then "(" ++ s ++ ")" else s
