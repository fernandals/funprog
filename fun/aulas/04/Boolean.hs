module Boolean where

data Boolean = T
             | F
             deriving (Eq,Show)

lor :: Boolean -> Boolean -> Boolean
lor x y = if x == T
             then T
             else y

land :: Boolean -> Boolean -> Boolean
land x y = if x == T
              then y
              else F

lnot :: Boolean -> Boolean
lnot F = T
lnot _ = F

-- :/ oops
ifthenelse :: Boolean -> a -> a -> a
ifthenelse T x _ = x
ifthenelse F _ y = y
