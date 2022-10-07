module Towards where

import Data.Char ( toUpper )

numbers :: [Int]
numbers = [1,2,3,4,5,6,7,8,9]

phrase :: [Char]
phrase = "Hello there"

scream :: [Char] -> [Char]
-- scream []     = []
-- scream (x:xs) = toUpper x : scream xs
-- scream xs = [toUpper x | x <- xs]

--scream xs = map' toUpper xs
-- pointless/point free style:
scream = map' toUpper

squareAll :: [Int] -> [Int]
-- squareAll []     = []
-- squareAll (x:xs) = (^2) x : squareAll xs
squareAll xs = [x^2 | x <- xs]

map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs

