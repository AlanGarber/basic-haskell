-- En una plantacion de pinnos, de cada arbol se conoce la altura expresada en metros. El peso de un pino se puede calcular a partir de la altura asi:
-- 3KG por cada centimetro hasta tres metros
-- 2KG por cada centimetro arriba de los tres metros


--Definí la función pesoPino, que recibe la altura de un pino en metros y devuelve su peso.
pesoPino :: Float -> Float
pesoPino altura
    | altura <= 3 = altura * 100 * 3
    | otherwise = (3 * 100 * 3) + ((altura - 3) * 100 * 2)  -- 3 metros * 100 cm * 3 kg + (altura - 3) * 100 cm * 2 kg

--Definí la función esPesoUtil, que recibe un peso en kg y responde si un pino de ese peso le sirve a la fábrica
esPesoUtil:: Float -> Bool 
esPesoUtil peso = peso >= 400 && peso <= 1000

-- Los pinos se usan para llevarlos a una fábrica de muebles, a la que le sirven árboles de entre 400 y 1000 kilos, un pino fuera de este rango no le sirve a la fábrica.
-- Definí la función sirvePino, que recibe la altura de un pino y responde si un pino de ese peso le sirve a la fábrica
sirvePino :: Float -> Bool
sirvePino altura = esPesoUtil (pesoPino altura)