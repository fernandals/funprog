module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral
    , Bool(..)
    , not
    , (&&)
    , (||)
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat = Zero | Succ Nat

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show Zero     = "O"
    show (Succ x) = "S" ++ (show x)

instance Eq Nat where

    (==) Zero Zero         = True
    (==) Zero (Succ _)     = False
    (==) (Succ _) Zero     = False
    (==) (Succ x) (Succ y) = (==) x y 

instance Ord Nat where

    (<=) Zero Zero         = True
    (<=) Zero (Succ _)     = True 
    (<=) (Succ _) Zero     = False
    (<=) (Succ x) (Succ y) = (<=) x y

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min Zero Zero         = Zero
    min Zero (Succ _)     = Zero
    min (Succ x) (Succ y) = min x y 

    max Zero Zero         = Zero
    max Zero (Succ x)     = Succ x
    max (Succ x) (Succ y) = max x y

isZero :: Nat -> Bool
isZero x = (x == Zero)

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred Zero   = Zero
pred (Succ x) = x

even :: Nat -> Bool
even Zero = True
even (Succ x) = odd x 

odd :: Nat -> Bool
odd Zero = False
odd (Succ x) = even x

-- addition
(<+>) :: Nat -> Nat -> Nat
(<+>) Zero Zero  = Zero
(<+>) x (Succ y) = Succ ((<+>) x y)
(<+>) (Succ x) y = Succ ((<+>) x y)

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
(<->) :: Nat -> Nat -> Nat
(<->) Zero _    = Zero
(<->) x    Zero = x
(<->) (Succ x) (Succ y) = (<->) x y

-- multiplication
(<*>) :: Nat -> Nat -> Nat
(<*>) (Succ Zero) y = y 
(<*>) x (Succ Zero) = x 
(<*>) (Succ x) y = (<*>) x ((<+>) y y)
(<*>) _ _ = Zero

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
(<^>) _ Zero = (Succ Zero)
(<^>) x (Succ Zero) = x
(<^>) x (Succ y) = (<^>) ((<*>) x x) y

-- quotient
(</>) :: Nat -> Nat -> Nat
(</>) x (Succ Zero) = x
(</>) x (Succ y) = (</>) ((<->) x (Succ y)) y
(</>) _ _ = Zero

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) x y = (<->) x ((<*>) y ((</>) x y))
-- divisao euclidiana tem algo coisado aq

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) = undefined

divides = (<|>)


-- x `absDiff` y = |x - y|
-- (Careful here: this - is the real minus operator!)
absDiff :: Nat -> Nat -> Nat
absDiff = undefined

(|-|) = absDiff

factorial :: Nat -> Nat
factorial = undefined

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg = undefined

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo = undefined


--
-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!
--

toNat :: Integral a => a -> Nat
toNat = undefined

fromNat :: Integral a => Nat -> a
fromNat = undefined


-- Obs: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
        | x < 0     = undefined
        | x == 0    = undefined
        | otherwise = undefined

