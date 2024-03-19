module Histograma where

    histograma :: [Int] -> String
    histograma [] = ""
    histograma [x] = "1" 
    histograma (xs) = ""

    matrix :: [Int] -> [[Int]]
    matrix (x:xs) = []

    myFilter :: [Int] -> Int -> [Int]
    myFilter xs n = filter (isMyNumber n) xs

    isMyNumber :: Int -> Int -> Bool
    isMyNumber n m = n == m

    myPrinter :: [Int] -> String
    myPrinter [] = ""
    myPrinter [x] = "x"
    myPrinter (x:xs) = "x \n" ++ myPrinter xs
