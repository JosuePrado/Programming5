{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module CreditCard where
    toDigitsRev :: Integer -> [Integer]
    toDigitsRev n  | n <= 0 = []
                | otherwise = mod n 10 : toDigitsRev(div n 10)

    toDigits :: Integer -> [Integer]
    toDigits n  = reverse (toDigitsRev n)

    doubleEveryOther :: [Integer] -> [Integer]
    doubleEveryOther xs = doubleEveryOtherAux xs 0

    doubleEveryOtherAux :: [Integer] -> Integer -> [Integer]
    doubleEveryOtherAux [] _ = []  
    doubleEveryOtherAux [x] _ = [x] 
    doubleEveryOtherAux (x:y:xs) n
        | even (fromIntegral (length xs)) = x * 2 : y : doubleEveryOtherAux xs (n+1)
        | otherwise = x : y * 2 : doubleEveryOtherAux xs (n+1)

    sumDigits :: [Integer] -> Integer                  
    sumDigits [] = 0
    sumDigits (x:xs)    | div x 10 <= 0 = x + sumDigits xs
                        | otherwise = sumDigitsAux x + sumDigits xs


    sumDigitsAux :: Integer -> Integer
    sumDigitsAux n  | div n 10 <= 0 = n 
                    | otherwise =  mod n 10 + sumDigitsAux (div n 10)

    validate :: Integer -> Bool
    validate n = mod (sumDigits(doubleEveryOther(toDigits n ))) 10 == 0