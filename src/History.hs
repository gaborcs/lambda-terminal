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

push :: a -> History a -> History a
push newPresent (History past present _) = History (present:past) newPresent []

goBack :: History a -> History a
goBack history@(History past present future) = case past of
    [] -> history
    nearest:rest -> History rest nearest (present:future)

goForward :: History a -> History a
goForward history@(History past present future) = case future of
    [] -> history
    nearest:rest -> History (present:past) nearest rest
