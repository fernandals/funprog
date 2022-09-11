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
--             deriving (Show)

instance (Eq Weekday) where
    Mon == Mon = True 
    Tue == Tue = True
    Wed == Wed = True
    Thu == Thu = True
    Fri == Fri = True
    Sat == Sat = True
    Sun == Sun = True
    _   == _   = False

instace (Show Weekday) where
    show Mon = "Monday"
    show Tue = "Tuesday"
    show Wed = "Wednesday"
    show Thu = "Thursday"
    show Fri = "Friday"
    show Sat = "Saturday"
    show Sun = "Sunday"

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


