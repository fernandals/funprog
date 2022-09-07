{- 
    First steps with Haskell
    2nd Chapter
-}

double x = x + x

quadruple x = double (double x)

-- Factorial to a positive integer:
factorial x = product [1..x]

-- Average of a list of integers:
average ns = sum ns `div` length ns
