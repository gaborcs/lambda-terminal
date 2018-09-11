module Util where

getItemAtIndex :: [a] -> Int -> Maybe a
getItemAtIndex xs index = case xs of
    [] -> Nothing
    x:xs' -> if index == 0 then Just x else getItemAtIndex xs' $ index - 1
