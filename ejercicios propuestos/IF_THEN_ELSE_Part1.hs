module IF_THEN_ELSE_Part1 where

--(1)
-- Ingrese dos números enteros y determine el mayor de los dos (haga uso de los 
-- operadores condicionales).

mayor:: (Int, Int) -> Int
mayor (a, b) = if a > b then a else b


--(2)
-- El pago de la cuota moderadora de una EPS, depende del valor del sueldo del 
-- afiliado, así: Si el sueldo es inferior a $900.000, la cuota es de $1.200, en 
-- caso contrario es de $2.400 pesos.

calcularCuota :: Float -> Float
calcularCuota sueldoAfiliado = if sueldoAfiliado < 900000 then 1200 else 2400

--(3)
-- Determine la categoría a la que pertenece un producto, de acuerdo a lo 
-- siguiente: Si el código del producto es menor o igual a 450, el producto es de 
-- categoría ‘A’, en caso contrario es de categoría ‘B’

determinarCategoria :: Int ->  Char
determinarCategoria codigoProducto = if codigoProducto <= 450 then 'A' else 'B'

--(4)
-- El código de un estudiante consta de 5 cifras, la sede en la que estudia se 
-- determina por el último dígito de su código, de la siguiente manera:
-- Si el código termina en una cifra menor o igual a 4, el estudiante pertenece a la 
-- sede “Villa del Rosario”, en caso contrario, pertenece a la sede “Cúcuta”.

obtenerUltimoDigito :: Int -> Int
obtenerUltimoDigito codigo = codigo `mod` 10

verificarSede :: Int -> String
verificarSede codigo = if(obtenerUltimoDigito (codigo) <= 4 ) then "Villa del Rosario" else "Cucuta"

--(5)
-- Dadas las 3 notas de parciales de un estudiante en la asignatura de Cálculo: 
-- parcial1(35%), parcial2(35%), parcial3(30%), calcular su nota definitiva.
-- Para aprobar la asignatura se requiere una nota igual o superior a 3.0. Determine 
-- si el estudiante aprueba o reprueba la asignatura.

calcularNotaDefinitiva :: (Float, Float, Float) -> Float
calcularNotaDefinitiva (parcial1, parcial2, parcial3) = parcial1 * 0.35 + parcial2 * 0.35 + parcial3 * 0.3

evaluarAprobacion :: (Float, Float, Float) -> String
evaluarAprobacion (parcial1, parcial2, parcial3) = if calcularNotaDefinitiva(parcial1, parcial2, parcial3) >= 3.0
then "aprueba"
else "reprueba"


--Ejercicios con condicionales compuestas

--(1)
--Para pertenecer al equipo de Taekwondo del colegio, el estudiante debe ser 
--mayor de 10 años y tener un promedio de notas igual o superior a 8.0 / 10.0

verificarRequisitos :: (Integer, Double) -> Bool
verificarRequisitos (edad, promedioNotas) = if (edad > 10 && promedioNotas >= 8.0) then True else False

--(2)
--Los productos de una empresa están codificados con un numero de 3 cifras. En 
--algunos de ellos se han encontrado fallas de producción, lo que se ha hecho 
--relevante en su código. Si el digito central del código del producto está entre 5 y 
--7 (incluido el cero), el producto NO es defectuoso, en caso contrario lo es.

obtenerDigitoCentral :: Int -> Int
obtenerDigitoCentral codigo = (codigo `mod` 100) `div` 10

evaluarCalidadProducto :: Int -> String
evaluarCalidadProducto codigo = if obtenerDigitoCentral(codigo) == 0
                                then  "No defectuoso"
                                else if obtenerDigitoCentral(codigo) <= 7
                                     then if obtenerDigitoCentral(codigo) >=5
                                          then "No defectuoso"
                                          else "Defectuoso"
                                     else "Defectuoso"

--(3)
--Determine si un año es bisiesto o no. Un año bisiesto es múltiplo de 4; los años 
--que son múltiplos de 100, serán bisiestos si son divisibles también por 400, en 
--caso contrario no lo serán. (1600 es bisiesto, pero 1800 no lo es).

esBisiesto :: Int -> Bool
esBisiesto año = if((año `mod` 4) == 0)
                 then if((año `mod` 100) == 0)
                      then if((año `mod` 400) == 0)
                           then True
                           else False
                      else True
                 else False

--(4)
--Un producto se vende con descuento si las cantidades compradas oscilan entre 
--15 y 40, o entre 75 y 85.
esConDescuento :: Int -> Bool
esConDescuento cantidadProducto = if cantidadProducto > 75 
                                  then if cantidadProducto < 85
                                       then True
                                       else False
                                  else if cantidadProducto > 15
                                       then if cantidadProducto < 40
                                            then True
                                            else False
                                       else False

--(5)
--La nota definitiva por un estudiante se considera “Notable”, si está entre 4.5 y 
--4.9, en caso contrario no lo es.
esNotable :: Float -> Bool
esNotable nota = if(nota > 4.5)
                 then if (nota < 4.9)
                      then True 
                      else False
                 else False