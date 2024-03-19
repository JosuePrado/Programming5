module IsPrime where

--Verificar si un numero es primo
esPrimo :: Int -> Bool
esPrimo n = null [x | x <- [2..n-1], mod n x == 0]

--Obtener todos los numeros de primos hasta el numero dado
getPrimos :: Int -> [Int]
getPrimos n = [ x | x <- [2..n], esPrimo x ]