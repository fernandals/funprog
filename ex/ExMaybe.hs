module ExMaybe where

-- Do not alter this import!
import Prelude hiding ( maybe, Maybe(..) )
import qualified Data.Maybe as M

data Maybe a = Nothing | Just a
    deriving (Show, Eq, Ord)

catMaybes :: [Maybe a] -> [a]
catMaybes xs = [ fromJust x | x <- xs, isJust x ]

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust _        = error "Nothing."

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just y) = y
fromMaybe x Nothing  = x

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

isNothing :: Maybe a -> Bool
isNothing m = not $ isJust m 

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x 

-- se tiver um Nothing ai buga tudo
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f []     = []
mapMaybe f (x:xs) = fromJust (f x) : mapMaybe f xs 

maybe :: b -> (a -> b) -> Maybe a -> b
maybe _ f (Just y) = f y 
maybe x _ _        = x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x] 

tryToModifyWith :: [Maybe (a -> a)] -> [a] -> [a]
tryToModifyWith [] _          = []
tryToModifyWith _  []         = []
tryToModifyWith (m:ms) (x:xs) = (fromJust m) x : tryToModifyWith ms xs 

--tryToModifyWith ms xs = same (catMaybes ms) xs
--where same (n:ns) (y:ys) = (fromJust n) y : same ns ys 
