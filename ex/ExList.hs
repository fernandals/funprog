module ExList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

-- to use a function from a qualified import
-- you need to prefix its name with its alias
-- and a dot:
-- P.head   C.toUpper   etc.
-- I import these for you to test the original functions on ghci:
-- ghci> :t C.toUpper
-- C.toUpper :: Char -> Char
-- You MUST NOT use ANY of these in your code

head :: [a] -> a
head []    = error "Empty list!"
head (x:_) = x 

last :: [a] -> a
last [] = error "Empty list!"
last xs = xs !! (length xs - 1)

tail :: [a] -> [a]
tail []     = error "Empty list!"
tail (_:xs) = xs

null :: [a] -> Bool
null [] = True
null _  = False

length :: Integral i => [a] -> i
length []     = 0
length (_:xs) = 1 + length xs

sum :: Num a => [a] -> a
sum []     = 0
sum (x:xs) = x + sum xs

product :: Num a => [a] -> a
product []     = 1
product (x:xs) = x * product xs

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x] 

(++) :: [a] -> [a] -> [a]
xs     ++ [] = xs
[]     ++ xs = xs
(x:xs) ++ ys = x : (xs ++ ys)

-- right-associative for performance!
-- (what?!)
infixr 5 ++

snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc x ys = ys ++ [x]

(<:) :: [a] -> a -> [a]
(<:) [] x = snoc x []
(<:) xs y = snoc y xs

(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum []  = error "Empty list :/"
minimum (x:xs) = min x xs
                 where min a bs
                         | null bs     = a
                         | head bs < a = min (head bs) (tail bs)
                         | otherwise   = min a (tail bs)

maximum :: Ord a => [a] -> a
maximum [] = error "Empty list ;("
maximum (x:xs) = max x xs
                 where max a bs
                         | null bs     = a
                         | head bs > a = max (head bs) (tail bs)
                         | otherwise   = max a (tail bs)

take :: Int -> [a] -> [a]
take 0 _      = []
take x (y:ys) = y : take (x-1) ys

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop x ys = drop (x-1) (tail ys) 

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f []     = []
takeWhile f (x:xs) = if (f x) then x : (takeWhile f xs)
                     else []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile f []         = []
dropWhile f all@(x:xs) = if (f x) then dropWhile f xs
                         else all

tails :: [a] -> [[a]]
tails xs = drops 0 xs
           where drops n ys
                   | n == length ys = drop n ys : []
                   | otherwise      = drop n ys : drops (n+1) ys

init :: [a] -> [a]
init [] = []
init xs = take (length xs - 1) xs

inits :: [a] -> [[a]]
inits xs = takes 0 xs
           where takes n ys 
                   | n == length ys = take n ys : []
                   | otherwise      = take n ys : takes (n+1) ys

-- subsequences
--subsequences :: [a] -> [[a]]
--subsequences [] = [[]]
--subsequences xs = 

any :: (a -> Bool) -> [a] -> Bool
any f []     = False
any f (x:xs) = if f x then True
               else any f xs

all :: (a -> Bool) -> [a] -> Bool
all f []     = True
all f (x:xs) = if f x then all f xs
               else False

and :: [Bool] -> Bool
and []     = True
and (x:xs) = if x then and xs
             else False

or :: [Bool] -> Bool
or []     = False
or (x:xs) = if x then True
            else or xs

concat :: [[a]] -> [a]
concat [] = []
concat x  = (head x) ++ concat (tail x)

elem :: Eq a => a -> [a] -> Bool
elem x ys = any (x==) ys

elem' :: Eq a => a -> [a] -> Bool
elem' x []     = False
elem' x (y:ys) = if (x == y) then True
                 else elem' x ys

(!!) :: [a] -> Int -> a
[]     !! _ = error "Out of range..."
(x:_)  !! 0 = x
(_:xs) !! y = xs !! (y-1)

filter :: (a -> Bool) -> [a] -> [a]
filter f []     = []
filter f (x:xs) = if (f x) then x : filter f xs
                  else filter f xs

map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs

cycle :: [a] -> [a]
cycle (x:xs) = x : xs ++ cycle (x:xs)

repeat :: a -> [a]
repeat x = x : repeat x

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : (replicate (n-1) x)

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf xs ys = or $ map (xs ==) (inits ys)

-- isInfixOf
-- usar subsequences nesse aqui
-- or $ map (xs ==) (subsequences ys)

isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf xs ys = or $ map (xs ==) (tails ys)

zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys) = [(x,y)] +++ zip xs ys
zip _      _      = []

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (x:xs) (y:ys) = [f x y] +++ zipWith f xs ys
zipWith f _      _      = []

intercalate :: [a] -> [[a]] -> [a]
intercalate [] xs  = concat xs
intercalate xs []  = []
intercalate _  [[x]] = [x]
intercalate xs ys  = concat $ map (++xs) (init ys) <: last ys

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : (nub . filter (x/=)) xs

splitAt :: Int -> [a] -> ([a], [a])
splitAt x ys = (take x ys,
                take ((length ys)-x) (reverse ys)) 

break :: (a -> Bool) -> [a] -> ([a],[a])
break _ [] = ([],[])
--break p xs = (filter p xs, filter naosei xs)

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome = undefined
-- nub ++ length ?

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

