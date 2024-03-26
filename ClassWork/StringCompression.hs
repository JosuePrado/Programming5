module StringCompression where

    stringCompression :: String -> Int -> String 
    stringCompression [] _ = ""
    stringCompression [x] n | n == 0 = [x]
                            | otherwise = ""
    stringCompression (x:y:xs) n    | x == y = stringCompression (y:xs) (n+1) 
                                    | n /= 0 = show (n + 1) ++ stringCompression (x:y:xs) 0
                                    | otherwise = x : stringCompression (y:xs) n