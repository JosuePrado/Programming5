module RotateString where

    subArr :: String -> Int -> String
    subArr xs n | n < length xs - 1 = (xs !! (n+1)) : subArr xs (n+1)
                | otherwise = ""

    subArrRev :: String -> Int -> Int -> String
    subArrRev xs n m    | n > m = head xs : subArrRev (tail xs) (n+1) (m+1)
                        | otherwise = ""