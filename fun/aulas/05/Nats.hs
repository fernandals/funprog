module MyNat where

data Nat = Zero | Succ Nat
--    deriving (Show)

instance (Show Nat) where
    show Zero     = "O"
    -- show (Succ x) = 'S' : show x
    show (Succ x) = "S" ++ show x 

plus :: Nat -> Nat -> Nat
plus n Zero   = n
plus n (Succ m) = Succ (plus n m)


