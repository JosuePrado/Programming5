module Data where

    data Shape = Circle Float 
                | Rect Float Float
                deriving (Show)


    square :: Float -> Shape
    square n = Rect n n

    area :: Shape -> Float
    area ( Circle r) = pi * r^2
    area (Rect x y) = x * y
    
    