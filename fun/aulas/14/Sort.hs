module Sort
    ( sort
    , msort
    , qsort
    , isort
    ) where

sort :: Ord a => [a] -> [a]
sort = undefined 

--------- Merge Sort

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

halve :: [a] -> ([a], [a])
halve []       = ([], [])
halve [x]      = ([x], [])
halve (x:y:xs) = (x:lxs, y:rxs)
  where
    (lxs, rxs) = halve xs

-------- Quick Sort

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (w:xs) = qsort small ++ [w] ++ qsort large
  where
    small = [ x | x <- xs, x <= w ]
    --small = filter (<=w) xs
    large = [ x | x <- xs, x > w ]
    --large = filter (>w) xs

------- Insertion Sort

isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)

-- ASSUMPTION: ys is sorted
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x ys@(y:ys')
  | x <= y = x : ys
  | otherwise = y : insert x ys'

-----------------------------------------------------
  -- TESTING

sorted :: Ord a => [a] -> Bool
sorted (x:y:xs) = x <= y && sorted (y:xs)
sorted _        = True

prop_qsortLength xs = length xs == length (qsort xs)
prop_qsortSorts xs  = sorted (qsort xs)
prop_qsortQsort xs  = qsort xs == qsort (qsort xs)


