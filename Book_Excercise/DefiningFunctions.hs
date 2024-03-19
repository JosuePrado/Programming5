module DefiningFunctions where

    -- 5
    andExpresion :: Bool -> Bool -> Bool
    andExpresion x y =
        if x == True
            then if y == True
                then True
                else False
            else False

    -- 6
    andAlternative :: Bool -> Bool -> Bool
    andAlternative a b  =
        if a == True
            then b
        else False

    --7
    mult :: Int -> (Int -> (Int -> Int))
    mult x = \y -> (\z -> x * y * z)

    -- 8
    luhnDouble :: Int -> Int
    luhnDouble x    | 2 * x > 9 = 2 * x - 9
                    | otherwise = 2 * x

    luhn :: Int -> Int -> Int -> Int -> Bool
    luhn a b c d = mod (luhnDouble a + b + luhnDouble c + d) 10 == 0