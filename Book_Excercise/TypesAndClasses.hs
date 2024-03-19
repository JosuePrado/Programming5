module TypesAndClasses where

second :: [a] -> a
second xs = head (tail xs)

swap :: (b, a) -> (a, b)
swap (x, y) = (y, x)

double :: Num a => a -> a
double x = x * 2

palindrome :: [String] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f  (f x)

-- Ejercicio 4
-- ghci> :t second 
-- second :: [a] -> a
-- ghci> :t swap
-- swap :: (b, a) -> (a, b)
-- ghci> :t double
-- double :: Num a => a -> a
-- ghci> :t palindrome 
-- palindrome :: [String] -> Bool
-- ghci> :t twice
-- twice :: (t -> t) -> t -> t

-- Ejercicio 5
-- En Haskell, no es posible instanciar una función con el tipo Eq porque el tipo Eq es una clase de tipos, no un tipo en sí mismo.
-- Por ejemplo, la clase Eq proporciona la operación (==) que se utiliza para verificar la igualdad entre dos valores del mismo tipo.

-- Es posible hacer que una función sea una instancia de la clase Eq en Haskell cuando se define una instancia específica para un tipo 
--    de dato definido por el usuario que representa funciones y que proporciona una forma de comparar funciones de manera significativa.