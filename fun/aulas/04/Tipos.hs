module Tipos where

-- Primitive
-- Char; Int; Integer; Float; Double

-- [Char]; (,); []; (->)


data Weekday = Mon
             | Tue
             | Wed
             | Thu
             | Fri
             | Sat
             | Sun
             deriving (Show)

nextDay :: Weekday -> Weekday
nextDay Mon = Tue
nextDay Tue = Wed
nextDay Wed = Thu
nextDay Thu = Fri
nextDay Fri = Sat
nextDay Sat = Sun
nextDay Sun = Mon

nextWorkingDay :: Weekday -> Weekday
nextWorkingDay Mon = Tue
nextWorkingDay Tue = Wed
nextWorkingDay Wed = Thu
nextWorkingDay Thu = Fri
nextWorkingDay _   = Mon 

nextWorkingDay' :: Weekday -> Weekday
nextWorkingDay' Fri = Mon 
nextWorkingDay' Sat = Mon
nextWorkingDay' Sun = Mon
nextWorkingDay' x   = nextDay x 


