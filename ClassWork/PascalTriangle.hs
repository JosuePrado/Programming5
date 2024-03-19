module Main where

    pascalAux :: [Int] -> [Int]
    pascalAux [_] = [1]
    pascalAux (x:xs) = (x + head xs) : pascalAux xs

    pascal :: Int -> [Int] -> [[Int]]
    pascal n xs  | n > 0 =  xs : pascal (n-1) (pascalAux (0 : xs))
                    | otherwise = []

    printPascal :: [[Int]] -> String
    printPascal [] = []
    printPascal (x:xs) = show x ++ "\n" ++ printPascal xs

    main :: IO ()
    main = putStrLn (printPascal (pascal 10 [1]))