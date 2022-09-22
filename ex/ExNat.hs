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
    (==) Zero _            = False
    (==) _    Zero         = False
    (==) (Succ x) (Succ y) = x == y 

instance Ord Nat where

    (<=) Zero Zero         = True
    (<=) Zero _            = True 
    (<=) _    Zero         = False
    (<=) (Succ x) (Succ y) = x <= y

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min Zero     _        = Zero
    min _        Zero     = Zero
    min (Succ x) (Succ y) = Succ (min x y)

    max Zero     x        = x
    max x        Zero     = x
    max (Succ x) (Succ y) = max x y

isZero :: Nat -> Bool
isZero x = (x == Zero)

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred Zero     = Zero
pred (Succ x) = x

even :: Nat -> Bool
even Zero     = True
even (Succ x) = odd x 

odd :: Nat -> Bool
odd Zero     = False
odd (Succ x) = even x

-- addition
(<+>) :: Nat -> Nat -> Nat
(<+>) Zero x     = x
(<+>) x Zero     = x
(<+>) x (Succ y) = Succ (x <+> y)

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
(<->) :: Nat -> Nat -> Nat
(<->) Zero _            = Zero
(<->) x    Zero         = x
(<->) (Succ x) (Succ y) = (<->) x y

-- multiplication
(<*>) :: Nat -> Nat -> Nat
(<*>) (Succ Zero) x = x 
(<*>) x (Succ Zero) = x 
(<*>) x (Succ y)    = x <+> (x <*> y)
(<*>) _ _           = Zero

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
(<^>) _ Zero        = (Succ Zero)
(<^>) Zero _        = Zero
(<^>) x (Succ Zero) = x
(<^>) x (Succ y)    = (<^>) ((<*>) x x) y

-- quotient
(</>) :: Nat -> Nat -> Nat
(</>) x Zero        = error "division by zero."
(</>) Zero x        = Zero
(</>) x (Succ Zero) = x
(</>) x y           = if x < y then Zero
                      else Succ ( (x <-> y) </> y )

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) Zero x = Zero </> x
(<%>) x Zero = x </> Zero
(<%>) x y    = if x < y then x
               else (x <-> y) <%> y
    
-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) x y = if (x <%> y == Zero) then True 
            else False

divides = (<|>)


-- x `absDiff` y = |x - y|
-- (Careful here: this - is the real minus operator!)
absDiff :: Nat -> Nat -> Nat
absDiff Zero x            = x
absDiff x Zero            = x
absDiff (Succ x) (Succ y) = absDiff x y

(|-|) = absDiff

factorial :: Nat -> Nat
factorial Zero     = (Succ Zero)
factorial (Succ x) = (Succ x) <*> (factorial x)

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg Zero = Zero
sg _    = Succ Zero

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo = undefined


--
-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!
--

toNat :: Integral a => a -> Nat
toNat 0 = Zero
toNat (Succ x) = Succ (toNat x)

fromNat :: Integral a => Nat -> a
fromNat Zero = 0
fromNat (Succ x) = 1 + (fromNat x)


-- Obs: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
        | x <= 0    = Zero
        | otherwise = toNat x 

