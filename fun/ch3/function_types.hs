zeroto :: Int -> [Int]
zeroto n = [0..n]

add :: (Int,Int) -> Int
add (x,y) = x+y

-- Curried add function
add' :: Int -> (Int -> Int)
add' x y = x+y
