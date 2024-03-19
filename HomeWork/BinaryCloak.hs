module BinaryCloak where

    binaryToDecimal :: [Int] -> Int -> Int
    binaryToDecimal [] _ = 0
    binaryToDecimal (x:xs) n = x * (2^n) + binaryToDecimal xs (n+1)

    inputReader :: String -> [String]
    inputReader n = lines n


    mylists :: [String] -> Int -> String
    mylists [] _ = []
    mylists (x:xs) n  = (x !! n) : (head xs !! n) : mylists xs (n+1)