--Son funciones que reciben funciones como argumentos y devuelven funciones como resultado

--Una funcion que recibe una funcion que va de a en b y devuelve una lista de b´s, y una lista de a y devuelve una lista de b 
map' :: (a -> b) -> [a] -> [b]
map' f [] = []

--Una funcion que recieb a y devuelve un bool, y una lista de a y devuelve una lista de a
filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []    

--Una funcion que recibe a y devuelve bool, y una lista de a y devuelve bool
all' :: (a -> Bool) -> [a] -> Bool   
all' f [] = True --Devuelve True si toda la lista cumpla la condicion

--Una funcion que recibe a y devuelve bool, y una lista de a y devuelve bool
any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False --Devuelve True si alguno de los elementos de la lista cumple la condicion



totalCaracteresLista :: [String] -> Int
totalCaracteresLista lista = sum (map length lista)

esPalabraLarga :: String -> Int -> Bool
esPalabraLarga palabra x = length palabra > x

cantPalabrasconMasDeXLetras :: Int-> [String] -> Int
cantPalabrasconMasDeXLetras x lista = length  (filter (`esPalabraLarga` x) lista)

cantPalabrasconMasDe10Letras :: [String] -> Int
cantPalabrasconMasDe10Letras lista = length (filter (>10) ( map length lista))

--Composicion de funciones

-- (.) :: (b -> c) -> (a -> b) -> a -> c

--Ejemplo
doble :: Int -> Int 
doble x = x * 2

cuadrado :: Int -> Int
cuadrado x = x * x

doblecuadrado :: Int -> Int
doblecuadrado = doble . cuadrado

lonigtudPalabras :: [String] -> [Int]
lonigtudPalabras = map length 

caracteresTotales :: [String] -> Int
caracteresTotales = sum . lonigtudPalabras

{-
Ejemplos orden superior

f :: ((t -> a) -> a) -> (t -> a) -> t -> (t -> a) -> a
f a b c
    | a c > b c = a
    | otherwise = b
(f doble triple 2) 10 --Devuelve 30

losDosPrimeros = take 2
losDosPrimeros [1,2,3,4,5] --Devuelve [1,2]
-}