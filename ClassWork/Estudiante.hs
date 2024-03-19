module Estudiante where
    
    data Estudiante = Estudiante {
        nombreCompleto :: String,
        apellidos :: String
    } deriving (Show)

    estudiantes :: [Estudiante]
    estudiantes = [Estudiante "Juan Carlos" "Perez Lopez"] 
