module Recursive where

    -- 5
    myLength :: [Int] -> Int
    myLength [] = 0
    myLength (x:xs) = 1 + myLength xs

    myDrop :: Int -> [Int] -> [Int]
    myDrop _ [] = []
    myDrop _ [x] = [x]
    myDrop n (x:xs) | n == x = myDrop n xs
                    | otherwise = x : myDrop n xs
    
    myInit :: [Int] -> [Int]
    myInit [] = []
    myInit [x] = []
    myInit (x:xs) = x : myInit xs 

    --6 Without looking at the definitions from the standard prelude, define the following library functions
    --on lists using recursion.
    myAnd :: [Bool] -> Bool
    myAnd [x] = x 
    myAnd (x:xs) = x && myAnd xs

    myConcat :: [[a]] -> [a]
    myConcat [] = []
    myConcat (x:xs) = x ++ myConcat xs

    myReplicate :: Int -> a -> [a]
    myReplicate n x | n == 0 = []
                    | otherwise = x : myReplicate (n - 1) x

    select :: [a] -> Int -> a
    select (x:xs) n | n == 0 = x
                    | otherwise = select xs (n - 1)

    myBelongs :: Eq a => a -> [a] -> Bool
    myBelongs _ [] = False
    myBelongs n (x:xs)  | x == n = True
                        | otherwise = myBelongs n xs

    -- 7.
    -- Define a recursive function merge :: Ord a
    -- => [a] -> [a] -> [a]
    -- lists to give a single sorted list. For example:
    -- that merges two sorted
    -- > merge [2,5,6] [1,3,4]
    -- [1,2,3,4,5,6]

    merge :: Ord a => [a] -> [a] -> [a]
    merge [] [] = []
    merge (x:xs) (y:ys) | x > y = y : x : merge xs ys
                        | otherwise = x : y : merge xs ys