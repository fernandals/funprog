module Desafios where

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = concat . select p
    where
        select p xs =
            [ if p x then [x] else [] | x <- xs ] 

--filter = ... concat ...
--filter [1, 2, 3, 4, 5, 6, 7]
--     = [[], [2], [3], [], [5], [], [7]]
--     = [ 2, 3, 5, 7 ]

sorted' :: Ord a => [a] -> Bool 
sorted' xs'@(_:xs) = and (zipWith (<=) xs' xs)
