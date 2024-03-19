module ListComprehensions where

    -- 6
    perfects :: Int -> [Int]
    perfects n = [x | x <- [1..n], x == factors x]

    factors :: Int -> Int
    factors n = factorsAux [x | x <- [1..(n-1)], mod n x == 0]

    factorsAux :: [Int] -> Int
    factorsAux = sum

    --7
    --myConcat :: 
    --myConcat 

    --8 
    positions :: Int -> [Int] -> [Int]
    positions x xs = [i | i <- find x (zip xs [0..length xs - 1]), x == i]

    find :: Int -> [(Int,Int)] -> [Int]
    find k t = [v | (x,v) <- t, x == k]