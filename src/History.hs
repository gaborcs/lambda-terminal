{-# LANGUAGE TemplateHaskell #-}

module History where

import Control.Lens

data History a = History
    { _past :: [a] -- from nearest to furthest from the present
    , _present :: a
    , _future :: [a] -- from nearest to furthest from the present
    }
makeLenses ''History

create :: a -> History a
create present = History [] present []

step :: (a -> a) -> History a -> History a
step modifyPresent (History past present _) = History (present:past) (modifyPresent present) []

push :: a -> History a -> History a
push newPresent = step $ const newPresent

goBack :: History a -> History a
goBack history@(History past present future) = case past of
    [] -> history
    nearest:rest -> History rest nearest (present:future)

goForward :: History a -> History a
goForward history@(History past present future) = case future of
    [] -> history
    nearest:rest -> History (present:past) nearest rest
