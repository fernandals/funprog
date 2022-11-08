module Talvez where

import Data.Maybe

firstThat :: (a -> Bool) -> [a] -> Maybe a
firstThat p []     = Nothing
firstThat p (x:xs) = if p x then Just x
                     else firstThat p xs

isGoodFirstThat :: (a -> Bool) -> (a -> Bool) -> [a] -> Maybe a
isGoodFirstThat p q xs
  | isNothing firstValue = Nothing
  | otherwise            = testValue p 
  where firstValue = firstThat q xs
        testValue p
          | p (fromJust firstValue) = firstValue
          | otherwise               = Nothing
