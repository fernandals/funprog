module Box where

---- construtor de tipo
------------ construtor de dados
data Box a = Box a
    deriving ( Show , Eq )

c :: Bool -> Box Bool
c x = Box x

no :: Box Bool -> Box Bool
no (Box True) = Box False
no (Box False) = Box True

bottom :: Bool 
bottom = bottom

boxBottom :: Box Bool 
boxBottom = Box bottom

--box :: a -> Box a
--box x = boxBottom
--box x = bottom
--box x = Box x
