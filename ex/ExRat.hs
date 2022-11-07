module ExRat
    ( rat
    , (//)
    , denominator
    , numerator
    ) where

type Numerator   = Integer
type Denominator = Integer

-- define Rat:
data Rat = Rat Numerator Denominator

instance Show Rat where
    show (Rat x y) = show x ++ "/" ++ show y

instance Eq Rat where
    Rat x y == Rat n m = x * m == y * n 

instance Num Rat where
    (+) x y =
        let newDenominator = lcm (denominator x) (denominator y)
            newNumerator z = numerator z * (newDenominator `div` denominator z)
        in rat (newNumerator x + newNumerator y) newDenominator
    
    (*) x y = rat (numerator x * numerator y) (denominator x * denominator y) 
    
    negate x = rat (negate $ numerator x) (denominator x)
    
    abs = undefined
    
    signum x = undefined
    fromInteger = undefined 

instance Ord Rat where
    compare = undefined

rat :: Integer -> Integer -> Rat
rat x y
  | y == 0    = error "Division by zero!"
  | otherwise = Rat x y 

(//) :: Rat -> Rat -> Rat
(//) x y = x * (rat (denominator y) (numerator y))

denominator :: Rat -> Integer
denominator (Rat x y) = y 

numerator :: Rat -> Integer
numerator (Rat x y) = x 

