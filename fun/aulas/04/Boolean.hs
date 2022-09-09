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

