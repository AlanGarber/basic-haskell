-- Obtener el factorial de un numero

factorial :: Int -> Int
factorial n
    | n == 0 = 1
    | n > 0 = n * factorial (n-1)
    | otherwise = error "No se puede calcular el factorial de un numero negativo"