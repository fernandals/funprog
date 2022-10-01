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
head []  = error "Empty list!"
head (x:_) = x 

tail :: [a] -> [a]
tail []    = error "Empty list!"
tail (_:xs) = xs

null :: [a] -> Bool
null [] = True
null _  = False

length :: Integral i => [a] -> i
length []     = 0
length (_:xs) = 1 + (length xs)

sum :: Num a => [a] -> a
sum []     = 0
sum (x:xs) = x + (sum xs)

product :: Num a => [a] -> a
product []     = 1
product (x:xs) = x * (product xs)

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = (reverse xs) ++ [x] 

(++) :: [a] -> [a] -> [a]
xs ++ [] = xs
[] ++ xs = xs
(x:xs) ++ ys = (x : (xs ++ ys))

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc x ys = ys ++ [x]

(<:) :: [a] -> a -> [a]
(<:) [] x = snoc x []
(<:) xs y = snoc y xs

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?)
infixl 5 +++

-- minimum :: Ord a => [a] -> a
-- maximum :: Ord a => [a] -> a

take :: Int -> [a] -> [a]
take 0 _      = []
take x (y:ys) = [y] ++ take (x-1) ys

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop x ys = reverse (take x (reverse ys)) 

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f []     = []
takeWhile f (x:xs) = if (f x) then [x] ++ (takeWhile f xs)
                     else []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile f []         = []
dropWhile f all@(x:xs) = if (f x) then dropWhile f xs
                         else all

-- tails

init :: [a] -> [a]
init [] = []
init xs = take (length xs - 1) xs

-- inits

-- subsequences

any :: (a -> Bool) -> [a] -> Bool
any f []     = False
any f (x:xs) = if f x then True
               else any f xs

all :: (a -> Bool) -> [a] -> Bool
all f [] = True
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

--concat :: [[a]] -> [a]
--concat x = (head x) ++ concat (tail x)

-- elem using the funciton 'any' above

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

-- (!!)

-- filter
-- map

-- cycle
-- repeat
-- replicate

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

-- zip
-- zipWith

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

