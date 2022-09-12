module Shapes where

data Color = Red | Green | Blue
        deriving ( Show , Eq )

--type RGBColor = (Int, Int, Int)

data RGBColor = RGBColor Double Double Double
        deriving ( Show , Eq )

data Point3D = Point3D Double Double Double
        deriving ( Show , Eq )

data Shape = Circle Double
           | Square Double
           | Rectangle Double Double
           deriving ( Show, Eq )

data Pair a b = Pair a b 
        deriving ( Show , Eq )

area :: Shape -> Double
area (Circle r)      = pi * (r^2)
area (Square s)      = s^2
area (Rectangle w h) = w * h

rot90 :: Shape -> Shape
rot90 (Rectangle w h) = Rectangle h w
rot90 x               = x 
 
height :: Shape -> Double
height (Circle r)      = 2*r 
height (Square s)      = s
height (Rectangle w h) = h

