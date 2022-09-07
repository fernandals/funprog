-- Using library functions
splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

-- Using condicional expressions
signum :: Int -> Int
signum n = if n < 0 then -1 else
            if n == 0 then 0 else 1

-- Using guarded equations
abs n   | n >= 0    = n
        | otherwise = -n

-- Using pattern matching
--- wildcard pattern (_)
(&&) :: Bool -> Bool -> Bool
True && b  = b
False && _ = False

--- tuples patterns
fst :: (a,b) -> a
fst (x,_) = x

--- list patterns (cons operator : )
head :: [a] -> a
head (x:_) = x

-- Lambda (\) expressions
add :: Int -> (Int -> Int)
add = \x -> (\y -> x + y)

--- Simplifying
odds :: Int -> [Int]
odds n = map f [0..n-1]
          where f x = x*2 + 1

odds' :: Int -> [Int]
odds' n = map (\x -> x*2 + 1) [0..n-1]

-- Sections
--- If # is an operatoe then
(#)   = \x -> (\y -> x # y)
(x #) = \y -> x # y
(# y) = zx -> x # y
