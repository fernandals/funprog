module Talvez where

import Data.Maybe

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

firstThat :: (a -> Bool) -> [a] -> Maybe a
firstThat p []     = Nothing
firstThat p (x:xs) = if p x then Just x
                     else firstThat p xs
--firstThat p = safeHead . filter p

maybeize :: (a -> b) -> Maybe a -> Maybe b
maybeize _ Nothing  = Nothing
maybeize f (Just x) = Just (f x)

isGoodFirstThat' :: (a -> Bool) -> (a -> Bool) -> [a] -> Maybe Bool
isGoodFirstThat' good p = maybeize good . firstThat p
