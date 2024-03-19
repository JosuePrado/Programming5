module Example where

-- Haskel por defecto usa las curried functions
concatenar3 :: String -> String -> String -> String
concatenar3 a b c = a ++ " " ++ b ++ " " ++ c

concatenarHola :: String -> String -> String
concatenarHola = concatenar3 "Hola"

concatenarHolaMundo = concatenarHola "Mundo"

--segundo ejemplo
sum :: Int -> Int -> Int
sum x y = x + y

--tercer ejemplo
multiply :: Int -> Int -> Int
multiply x y = x * y

--cuarto ejemplo 
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

-- quinto ejemplo
power :: Int -> Int -> Int
power base exponent = base ^ exponent

