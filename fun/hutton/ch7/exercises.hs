-- Q1
--
q1 :: (a -> Bool) -> (a -> b) -> [a] -> [b]
q1 p f = map f . filter p

-- Q2
--
all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

any' :: (a -> Bool) -> [a] -> Bool
any' p = not . null . filter p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p []     = []
takeWhile' p (x:xs) = if p x then x : takeWhile' p xs
                      else []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p []     = []
dropWhile' p (x:xs) = if p x then dropWhile' p xs
                      else (x:xs)

-- Q3
--
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) [] 

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr ((++) . check p) []
            where check p x = if p x then [x]
                              else []

-- Q4
--
dec2int :: [Int] -> Int
dec2int = foldl ((+) . (*10)) 0

-- Q5
--
--curry' :: ( (a,b) -> c ) -> ( a -> b -> c )
--curry' f (x,y) = 

