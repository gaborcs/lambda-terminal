module Util where

getItemAtIndex :: Int -> [a] -> Maybe a
getItemAtIndex index xs = case xs of
    [] -> Nothing
    x:xs' -> if index == 0 then Just x else getItemAtIndex (index - 1) xs'

removeItemAtIndex :: Int -> [a] -> [a]
removeItemAtIndex index xs = case xs of
    [] -> []
    x:xs' -> if index == 0 then xs' else x : removeItemAtIndex (index - 1) xs'

inParensIf :: Bool -> String -> String
inParensIf cond s = if cond then "(" ++ s ++ ")" else s
