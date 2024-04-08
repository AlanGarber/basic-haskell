{-
    Una empresa abre una cierta cantidad de sucursales y necesita contratar nuevos empleados. La cantidad de empleados para cada sucursal es la misma, y se calcula según el nombre de la empresa, de la siguiente manera:
    -Si la empresa es "Acme", son 10 empleados.
    -Si el nombre de la empresa termina con una letra menor que la con que empieza, son tantos empleados como letras intermedias (o sea, el nombre sin considerar la primera y la última letra).
        Por ejemplo "star", contrata 2 empleados por sucursal.
    -Si el nombre es capicúa y tiene cantidad par de letras, los empleados son el doble de la cantidad de letras intermedias.
        Por ejemplo, "NOXXON", son 8.
    -Si la cantidad de letras del nombre es divisible por 3 o por 7, la cantidad de empleados es la cantidad de copas del mundo ganadas por Argentina.
    -En cualquier otro caso, no se contratan empleados para cada sucursal
    El objetivo final es obtener la cantidad total de empleados que va a contratar una empresa
-}


esCapicua:: String -> Bool
esCapicua texto = texto == reverse texto 
esDivisible:: String -> Int -> Bool
esDivisible texto divisor = length texto `mod` divisor == 0

cantidadEmpleados :: String -> Int
cantidadEmpleados nombre 
    | nombre == "Acme" = 10
    | last nombre < head nombre = length nombre - 2
    | esCapicua nombre && esDivisible nombre 2 = (length nombre - 2) * 2
    | esDivisible nombre 3 || esDivisible nombre 7 = 3
    |otherwise = 0

-- En funciones con mas de un parametro, NO se usan ni parentesis ni comas
