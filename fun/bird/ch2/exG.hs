module MyDate where

--data Date = Date (Int, Int, Int)
type Date = (Int, Int, Int)

showDate :: Date -> String
showDate (d, m, y)
  | (d <= 0 || d > 31) || (m <= 0 || m > 12) || (y <= 0) = "Invalid Date!"
  | otherwise                                            = showDay d ++ " " ++ showMonth m ++ ", " ++ show y

showDay :: Int -> String
showDay x
  | x' == 1   = show x ++ "st" 
  | x' == 2   = show x ++ "nd"
  | x' == 3   = show x ++ "rd"
  | otherwise = show x ++ "th"
  where x' = x `mod` 10 
  
showMonth :: Int -> String
showMonth x
  | x == 1  = "January"
  | x == 2  = "February"
  | x == 3  = "March"
  | x == 4  = "April"
  | x == 5  = "May"
  | x == 6  = "June"
  | x == 7  = "July"
  | x == 8  = "August"
  | x == 9  = "September"
  | x == 10 = "October"
  | x == 11 = "November"
  | x == 12 = "December"

