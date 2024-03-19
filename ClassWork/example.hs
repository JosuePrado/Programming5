module Ejemplo where


nextNum n = n + 1

square n = n * n

suma:: Int -> (Int -> Int)
suma a b = a + b

esPar:: Int -> Bool
esPar n = mod n 2 == 0

cadena n = "asd $n"

mySplitAt :: Int -> [Int] -> ([Int], [Int])
mySplitAt n xs = (take n xs, drop n xs)

myABS :: (Ord a, Num a) => a -> a
myABS n | n >= 0    = n  
        | otherwise = -n

-- Guarded Equation
validPositiveNumber :: (Ord a, Num a) => a -> a 
validPositiveNumber n   | n < 0     = (n * (-1))
                        | n == 0    = 0
                        | otherwise = 1


--Pattern matching
negar:: Bool -> Bool
negar True = False
negar False = True 

esUno :: Int -> Bool
esUno n = True


--Lamda Function
add :: Int -> (Int -> Int)
add = \x -> (\y -> x * y)

myFuntions :: [Int -> Int -> Int]
myFuntions = [(+), (-), (*), div, suma]

getFuntion :: Char -> (Int -> Int -> Int)
getFuntion e    | e == '+'      = myFuntions !! 0
                | e == '-'      = myFuntions !! 1 
                | e == '*'      = myFuntions !! 2
                | e == '/'      = myFuntions !! 3
                | otherwise     = myFuntions !! 4

myExp :: Char -> (Int -> Int -> Int)
myExp = getFuntion 

-- Curried Function
-- Example ((multThree 1) 3) 4
multThree  :: Int -> (Int -> (Int -> Int))
multThree n m o = n * m * o

--Lista por compresion
-- (x^2 : x E [1..5]) En matematicas
-- [x^2 : x <- [1..5]] En haskkel


-- ++ sirve para concatenar arreglos
-- Quick Sort EZ
qSort :: [Int] -> [Int]
qSort [] = []
qSort (x:xs) = (qSort menor) ++ [x] ++ (qSort mayor)
                where 
                        menor = [me | me <- xs, me < x]
                        mayor = [ma | ma <- xs, ma >= x]

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

--Es Primo Tarea
esPrimo :: Int -> Bool
esPrimo n = null [x | x <- [2..isqrt n], n `mod` x == 0]


getPrimos :: Int -> [Int]
getPrimos n = [ x | x <- [1..n], esPrimo x ]

factorial 0 = 1
factorial n = n * factorial(n-1)

instartVal :: Ord a => a -> [a] -> [a]
instartVal x [] = [x]
instartVal x (y:ys) | x <= y = x : y : ys
                    | otherwise = y : instartVal x ys


myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y): myZip xs ys

--foldr usa de foldable 
mySum :: Num a => [a] -> a
mySum = foldr (*) 1

