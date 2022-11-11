module Coproduct where

import Prelude hiding (Either(..))

data Either a b = Left a
                | Right b
                deriving (Show, Eq)

h :: Either a b -> d
h (Left x)  = f x
h (Right x) = g y
    where
        f :: a -> d
        f x = Left x 
        g :: b -> d
        g y = Right y 


