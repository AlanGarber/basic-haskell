-- TODO MINUSCULA MENOS LA INICIAL DE LOS TIPOS DE DATOS

-- ':r' para recargar el archivo 
--definiendo dominio e imagen
doble :: Int -> Int
triple :: Int -> Int
cuadruple :: Int -> Int
cuadrado :: Float -> Float

-- definiendo funciones
doble x = x * 2
triple x = x * 3
cuadruple x = x *4
cuadrado x = x * x

apellido :: String -> String
apellido "Alan" = "Garber"
apellido "Luli" = "Szwimer"
apellido "Nico" = "Fishamn" 
apellido "Facu" = "Vila"
apellido alg = "Sin apellido"
-- en la terminal el valor se pone con comiiilas dobles tal cual esta declarado en apellido
-- el tipo de variable de la funcion se declara por las asignaciones que hagas

esCero :: Int -> Bool
esCero 0 = True
esCero x = False

function :: Int -> Int  
function x = doble x + triple x

inicial :: String -> Char
inicial palabra = head palabra

numberpi :: Float
numberpi = 3.14
superficie :: Float -> Float    
superficie radio = numberpi * cuadrado radio
