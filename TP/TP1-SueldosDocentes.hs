{-
    Calcular el sueldo de un docente universitario de UTN, dado su cargo, cantidad de horas que trabaja, y su antigüedad en años.
    El básico depende del cargo que tenga la persona, también tiene un adicional por antigüedad y luego varía proporcionalmente dependiendo de la cantidad de horas que trabaje.

    Básico: El básico por cargo (Noviembre 2023) es el siguiente:
    "titular": $ 149000
    "adjunto": $ 116000
    "ayudante": $ 66000
    (Son valores reales, aunque existen otros cargos, como asociado, jefe trabajos prácticos y ayudante de primera. Los seleccionados son los más representativos:  Titular es generalmente jefe de cátedra, Adjunto  es la mayoría de los profesores a cargo de un curso, Ayudante 2° son los ayudantes que son alumnos. Por ahora modelar sólo estos tres)
    Antiguedad: El porcentaje de incremento por antigüedad es:
    Entre 3 y 5 años -> 20% adicional 
    a partir de 5 años -> 30% adicional
    a partir de 10 años -> 50% adicional
    a partir de 24 años (máxima antigüedad) -> 120% adicional
    (Son datos reales, aunque simplificados, ya que existen más valores intermedios en la escala, de manera de cada 2 o 3 años varía levemente el porcentaje. Por ahora modelar sólo estos)
    Cantidad de horas: La proporcionalidad de horas se calcula asumiendo que el importe por cargo corresponde a 10 horas semanales. Si trabaja 10hs se paga el sueldo base. Si trabaja 30 hs, se paga el sueldo base por 3. El valor se redondea, de manera que si trabaja 22hs se paga el sueldo base por 2, y si trabaja 37 horas se paga el sueldo base por 4. No hay personas que trabajen menos de 5 horas ni más de 50.
    (Este cálculo está simplificado, pero es bastante aproximado; en realidad se redondea no sólo a cantidades enteras sino también a "medias" de manera que si alguien trabaja 16 hs, se multiplica por 1.5, pero por ahora no hace falta modelar estos casos)

    Ejemplo
    Un "adjunto" con 10 años de antigüedad trabajando 24hs cobra $116.000 * 150% * 2 = $348000
-}

sueldoBase :: String -> Float
sueldoBase "titular" = 149000
sueldoBase "adjunto" = 116000
sueldoBase "ayudante" = 66000

antiguedadPorcentaje :: Int -> Float
antiguedadPorcentaje antiguedad
    | antiguedad < 3 = 1
    | antiguedad < 5 = 1.2
    | antiguedad < 10 = 1.3
    | antiguedad < 24 = 1.5
    | otherwise = 2.2

multiplicadorHoras :: Int -> Float
multiplicadorHoras horas
    | horas < 5 =  0
    | horas >= 5 && horas <= 15 = 1
    | horas > 15 && horas <= 25 = 2
    | horas > 25 && horas <= 35 = 3
    | horas > 35 && horas <= 45 = 4
    | otherwise = 5


sueldoDocente :: String -> Int -> Int -> Float
sueldoDocente cargo horas antiguedad = sueldoBase cargo * antiguedadPorcentaje antiguedad * multiplicadorHoras horas

{-
    Averiguar a qué distancia de la línea de pobreza está un docente universitario

    Se conocen los costos de la canasta básica total, que es la que define la línea de pobreza, para familias de diferente composición, a noviembre 2023.
    1 integrante (adulto) 126000
    3 integrantes (2 adultos y 1 adulto mayor) 310000
    4 integrantes (2 adultos y 2 menores) 390000
    5 integrantes (2 adultos y 3 menores) 410000 

    Se cuenta con una función que calcula el salario docente a la misma fecha. (Una forma posibles de calcularlo, en este otro ejercicio)

    También se concen los porcentajes de aumento, tanto de la inflación como del salario, de  diciembre 2023 a febrero 2024, ambos inclusive:
    Inflación General (IPC): 71%
    Aumento salarial: 22%
    (Datos oficiales. El aumento salarial fue 6% en diciembre y 10% en febrero según acuerdo paritario anterior. Por decisión del estado nacional sin acuerdo partitario, en febrero se agrega 6% no acumulativo)

    Requerimiento:
    Obtener la diferencia entre los ingresos del docente y el costo de la canasta básica, que puede ser un valor positivo o negativo. Tomar como base valores precisos de un determinado momento  (noviembre 2023) y actualizarlos según los porcentajes de variación de la inflación y salarios producidos desde entonces a la actualidad.

    Ejemplo
    Un docente que en noviembre 2023 cobraba $348000, con una familia de 3 integrantes, teniendo en cuenta incrementos de 71% y 22%, en febrero 2024 está  $105540 por debajo de la línea de pobreza.
    El mismo docente, haciendo el calculo para el mismo noviembre (o sea, con ambos incrementos en 0%) estaba $38000 por sobre la linea de pobreza.
    Cuando estén disponibles los indices de inflación de marzo y el aumento salarial, en caso de haber, se podria volver a hacer el cálculo, a ver si la situación mejoró o empeoró.
-}

actualizarSueldo :: Float -> Float -> Float -> Float
actualizarSueldo sueldo inflacion aumento = sueldo * (1 + aumento)


inflacionFebrero = 0.71
aumentoFebrero = 0.22

actualizarCanastaBasica :: Int -> Float -> Float
actualizarCanastaBasica integrantes inflacion 
    | integrantes == 1 = 126000 * (1 + inflacion)
    | integrantes == 3 = 310000 * (1 + inflacion)
    | integrantes == 4 = 390000 * (1 + inflacion)
    | integrantes == 5 = 410000 * (1 + inflacion)
    | otherwise = 0

diferenciaCanastaBasicaActualizada :: String -> Int -> Int -> Int -> String -> Float
diferenciaCanastaBasicaActualizada cargo horas antiguedad integrantes mes 
    | mes == "noviembre" =  sueldoDocente cargo horas antiguedad  - actualizarCanastaBasica integrantes 0
    | mes == "febrero" =  actualizarSueldo  (sueldoDocente cargo horas antiguedad) inflacionFebrero aumentoFebrero   - actualizarCanastaBasica integrantes inflacionFebrero