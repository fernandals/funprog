module ExEither where

-- Do not alter this import!
import Prelude hiding ( either, Either(..) )
import qualified Data.Either as E

data Either a b = Left a | Right b
    deriving (Show, Eq)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight e = not $ isLeft e

lefts :: [Either a b] -> [a]
lefts xs = [ left x | x <- xs, isLeft x ]
           where left (Left n) = n
           -- only left can make it to the list

rights :: [Either a b] -> [b]
rights xs = [ right x | x <- xs, isRight x ]
            where right (Right n) = n

fromLeft :: a -> Either a b -> a
fromLeft _ (Left y) = y
fromLeft x _        = x

fromRight :: b -> Either a b -> b
fromRight _ (Right y) = y
fromRight x _         = x

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers es = (lefts es, rights es) 

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g (Left x)  = f x
either f g (Right x) = g x

