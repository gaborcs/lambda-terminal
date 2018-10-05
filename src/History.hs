module History where

data History a = History
    { past :: [a] -- from nearest to furthest from the present
    , present :: a
    , future :: [a] -- from nearest to furthest from the present
    }

create :: a -> History a
create present = History [] present []

push :: a -> History a -> History a
push newPresent (History past present _) = History (present:past) newPresent []

replacePresent :: a -> History a -> History a
replacePresent newPresent history = history { present = newPresent }

goBack :: History a -> History a
goBack history@(History past present future) = case past of
    [] -> history
    nearest:rest -> History rest nearest (present:future)

goForward :: History a -> History a
goForward history@(History past present future) = case future of
    [] -> history
    nearest:rest -> History (present:past) nearest rest
