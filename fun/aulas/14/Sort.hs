module Sort
    ( sort
    , msort
    , qsort
    , isort
    ) where

sort :: Ord a => [a] -> [a]
sort = undefined 

-- ASSUMPTION: xs and ys are sorted
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge xs@(x:xs') ys@(y:ys')
  | x <= y    = x : merge xs' ys
  | otherwise = y : merge xs  ys'

msort :: Ord a => [a] -> [a]
msort []  = []
msort [z] = [z]
msort zs  = merge (msort xs) (msort ys)
  where
    (xs, ys) = halve zs
--    (xs, ys) = splitAt midpoint zs
--    midpoint = length zs `div` 2

halve :: [a] -> ([a], [a])
halve [] = ([], []) 
halve [x] = ([x], []) 
halve (x:xs) = ( x : halfUp xs, []) 
  where halfUp ys@(y:ys')
          | null ys   = []
          | otherwise = y : halfUp (init ys)

-- halve [1,2,3,4,5,6,7,8] = ([1,2,3,4], [5,6,7,8])

qsort :: Ord a => [a] -> [a]
qsort = undefined

isort :: Ord a => [a] -> [a]
isort = undefined

