module Dias where

    type DiaNumeral = Int
    type DiaLiteral = String
    type DiaDelMes = Int
    type MesNumeral = Int
    type AnioNumeral = Int

    queDiaEs :: DiaNumeral -> DiaLiteral
    queDiaEs x = case x of 
                    1 -> "Domingo"
                    2 -> "Lunes"
                    3 -> "Martes"
                    4 -> "Miercoles"
                    5 -> "Jueves"
                    6 -> "Viernes"
                    7 -> "Sabado"
                    _ -> "No se que dia es"

    diaDelMes :: MesNumeral -> AnioNumeral -> DiaDelMes
    diaDelMes m a = let
                        esBisiensto = anioEsBisiesto a
                    in 
                        case m of 
                            1 -> 31
                            2 -> if esBisiensto then 29 else 28
                            3 -> 31
                            4 -> 30
                            5 -> 31
                            6 -> 30
                            7 -> 31
                            8 -> 31
                            9 -> 30
                            10 -> 31
                            11 -> 30
                            12 -> 31
                            _ -> -1
                    where 
                        anioEsBisiesto :: AnioNumeral -> Bool                    
                        anioEsBisiesto a    | mod a 4 == 0 && mod a 100 /= 0 = True
                                            | mod a 100 == 0 && mod a 400 == 0 = True
                                            | otherwise = False
                                            
                        anioEsBisiesto2 :: AnioNumeral -> Bool                    
                        anioEsBisiesto2 a = let 
                                                p = a `mod` 4 == 0
                                                q = a `mod` 100 /= 0
                                                r = a `mod` 400 == 0
                                            in 
                                                p && (not q || r)

