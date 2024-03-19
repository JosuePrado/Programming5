module Main where
import GHC.Base (VecElem(Int16ElemRep))


-- "II + III" = "V"
-- "II - III" = "-I"
-- II * III = VI
-- VI : III = II
-- V % III = II

roman::[(String, Integer)]
roman = [("I", 1), ("IV", 4), ("V", 5), ("IX", 9), ("X", 10), 
         ("XL", 40), ("L", 50), ("XC", 90), ("C", 100),
         ("CD", 400), ("D", 500), ("CM", 900), ("M", 1000) ]

roman2 = reverse roman

expr :: [(String, Integer -> Integer -> Integer)]
expr = [("+", (+)), ("-", (-)), ("", (*)), (":", div), ("%", mod)]

calRoman :: String -> String
calRoman input = result $ words input
        where 
            result :: [String] -> String
            result (x:y:z:xs) = numberToRoman (getFuntion y (romanToNumber x) (romanToNumber z))
                                where 
                                    getFuntion :: String -> (Integer -> Integer -> Integer)
                                    getFuntion x = snd $ head $ filter(\n -> fst n == x) expr

-- II
romanToNumber :: String -> Integer
            -- I:[I]
romanToNumber "" = 0
-- romanToNumber (x:xs) | length xs > 0 && findNumber (x: head xs:[]) == [] =  
romanToNumber (x:xs) | length xs > 0 && getRoman [x, head xs] =
                                getNumber [x, head xs] + romanToNumber (tail xs)
                     | otherwise = getNumber [x] + romanToNumber xs
                     where 
                        getRoman :: String -> Bool
                        getRoman x = findNumber x /= []

                        findNumber :: String -> [(String, Integer)]
                        findNumber x = [r |r <- roman, fst r == x]

                        getNumber :: String -> Integer 
                        getNumber x | getRoman x = snd $ head $ filter (\n -> fst n == x) roman
                                    | otherwise = 0

numberToRoman :: Integer -> String
numberToRoman num = convertToRoman num 0
                 
convertToRoman :: Integer -> Int ->  String
convertToRoman 0 _ = "" 
convertToRoman num pos | num >= snd (roman2 !! pos) = fst (roman2 !! pos) ++ convertToRoman (num - snd(roman2 !! pos)) pos  
                       | num < 0 = "El resultado de esta operacion no esta soportadoen el sistema Romano"
                       | otherwise = convertToRoman num (pos+1)
 
main :: IO ()
main = do strings <- getContents
          mapM_ putStrLn $ map calRoman $ lines strings

