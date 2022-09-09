-- Q1
--
halve :: [a] -> ([a],[a])
halve xs = (take ((length xs) `div` 2) xs, drop ((length xs) `div` 2) xs)

-- Q2
--
---third :: [a] -> a
third_a xs = head (tail (tail xs))
third_b xs = xs !! 2
third_c (_:_:x:_) = x

-- Q3
--
---safetail :: [a] -> [a]
safetail_a xs = if null xs then [] else tail xs
safetail_b xs   | null xs   = []
                | otherwise = tail xs
safetail_c []     = []
safetail_c (_:xs) = xs

-- Q4
--
(||) :: Bool -> Bool -> Bool
False || False = False
_ || _         = True

{-
True  || _ = True
False || b = b

b || b = b
_ || _ = True

b || c  | b == c    = b
        | otherwise = True
-}

-- Q5
--
--(&&) :: Bool -> Bool -> Bool
{-a && b = if a == True then
                if b == True then True
                else False
         else False-}

--Q6
--
--(&&) :: Bool -> Bool -> Bool
--a && b = if a == b then a else False

-- Q7
--
mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> \z -> x * y * z)

-- Q8
--
luhnDouble :: Int -> Int
luhnDouble x | x*2 > 9   = x*2 - 9
             | otherwise = x*2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn z y x w = if (sum ((map luhnDouble [z,x]) ++ [w,y]) `mod` 10 == 0) then True else False
