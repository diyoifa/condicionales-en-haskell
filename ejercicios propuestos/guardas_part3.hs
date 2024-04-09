module GUARDAS_Part3 where

--(1)
--Crear una función simple que te dará un mensaje diferente, de acuerdo al valor
--de tu IMC (índice de masa corporal). Tu IMC es igual a tu peso dividida por tu
--altura al cuadrado. Si tu IMC es menor que 18,5 tienes infrapeso. Si estas en
--algún lugar entre 18,5 y 25 eres Normal. Si tienes entre 25 y 30 tienes sobrepeso
--y si tienes más de 30 eres obeso.



evaluarIMCDado :: Double -> String
evaluarIMCDado imc | imc < 18.5 = "infrapeso"
               | imc < 25 = "normal"
               | imc < 30 = "sobrepeso"
               | otherwise = "obeso"

--(2)
--Modificar el ejercicio anterior, de tal manera que en lugar de dejar que el usuario
--tenga que calcular su propio IMC por su cuenta antes de llamar a la función,
--vamos a modificar la función para que tome la altura y el peso y lo calcule por el
--usuario.

calcularIMC :: (Float, Float) -> Float
calcularIMC (altura, peso) = peso/(altura*altura)

evaluarIMC :: (Float, Float) -> String
evaluarIMC (altura, peso) | calcularIMC(altura, peso) < 18.5 = "infrapeso"
                          | calcularIMC(altura, peso) < 25 = "normal"
                          | calcularIMC(altura, peso) < 30 = "sobrepeso"
                          | otherwise = "obeso"

--(3)
--Definir la función “mediano” tal que
--(mediano x y z) devuelve el número mediano
--de los tres números x, y z.
calcularMediano :: (Float, Float, Float) -> Float
calcularMediano (x, y, z) | x > y && x < z = x 
                          | y > x && y < z = y
                          | otherwise = z

--(4)
--Definir la función “cuadrante” tal que (cuadrante p) es cuadrante del punto p (se
--debe resolver igual, si p está sobre un eje).

--(5) 
--Sea F una función de los reales en los reales, definida por
-- x si x < -5
-- x + 3 si -5 <= x <= 5
-- x^2 - 2 si x > 5

fx :: Float -> Float
fx x | x < -5 = x 
     | x <= 5 = x + 3
     | otherwise = x*x - 2

--(6)
--En la central de abastos de Pamplona (bodegas donde se almacena al por
--mayor) se ha establecido un mecanismo para el ingreso de vehículos que
--permite el cobro de parqueadero según las siguientes condiciones: Al centro de
--abastos entran vehículos con dos propósitos, algunos entran a descargar
--productos y otros entran a cargar (estos son los dos tipos de servicio que presta
--el centro de abastos 1. Cargue y 2. Descargue). A los vehículos que entran a
--descargar se les cobra de acuerdo a la siguiente tabla.

--peso transportado |   menos de 10 toneladas  |10 o mas toneladas
--tipo producto     |                          |
-- -----------------------------------------------------------
--1.perecedero     	|10.000 pesos por tonelada |7.000 pesos por tonelada 
--2.No perecedero   |50.000 por todo 		   |5.000 por tonelada extra 

--A los vehículos que entran a cargar se les cobra por el tiempo que permanecen
--dentro del centro de abastos así: Las primeras dos horas son gratis para todos
--los vehículos que entran a cargar. Las horas extras que permanecen dentro del
--parqueadero se cobran a 2000 pesos la hora (siempre se cobra la hora
--completa) y tendrá un incremento del 25% si el camión mide más de 4 metros
--de largo. Hacer el análisis y diseño del programa que ayude al portero del centro
--de abastos para calcular el costo de parqueadero de uno de los camiones que
--hace uso de estos servicios.


--proposito (carga o descarga)
--carga -> horas -> altura
--descarga -> tipo producto -> toneladas

cobroCarga :: (Int, Float) -> Float
cobroCarga (horas, altura) | horas <= 2 = 0
                           | altura > 4 = fromIntegral  (horas - 2) * (2000 + 2000 * 0.25)
                           | otherwise = fromIntegral  (horas - 2) * 2000

cobroDescarga :: (String, Int) -> Float
cobroDescarga (tipoProducto, toneladas)
    | (tipoProducto == "perecedero" && toneladas < 10) = fromIntegral toneladas * 10000
    | (tipoProducto == "perecedero" && toneladas >= 10) = fromIntegral toneladas * 7000
    | (tipoProducto == "no perecedero" && toneladas < 10) = fromIntegral 50000
    | (tipoProducto == "no perecedero" && toneladas >= 10) = fromIntegral 50000 + fromIntegral (toneladas - 10) * 5000


--(7)
--Un café Internet cobra a sus usuarios de la siguiente forma.
----------------------------------------
-- |tiempo de navegacion | valor a pagar|
----------------------------------------
-- |de 1 a 15 minutos    | 500          |
-----------------------------------------
-- |de 16 a 30 minutos   | 1000         |
-----------------------------------------
-- |de 31 a 60 minutos   |1400          |
-----------------------------------------
--Superior a 60 minutos, 20 pesos el minuto adicional. Dado el tiempo de
--navegación de una persona (en minutos) determinar el valor a pagar.

cobrarInternet :: Int -> Int
cobrarInternet tiempoNavegacion | tiempoNavegacion <= 15 = 500 
                                | tiempoNavegacion <= 30 = 1000
                                | tiempoNavegacion <= 60 = 1400
                                | otherwise = 1400 + (tiempoNavegacion - 60) * 20


--(8)
--Dados los coeficientes de una ecuación cuadrática 𝑎𝑥
--2 + 𝑏𝑥 + 𝑐 = 0
--determinar sus raíces, sabiendo que
calcularRaizPositiva :: (Int, Int, Int) -> Float
calcularRaizPositiva (a,b,c) = ((-b) + sqrt(b * b - 4 * a * c)) / (2 * a)

calcularRaizNegativa :: (Int, Int, Int) -> Float
calcularRaizNegativa (a,b,c) = ((-b) - sqrt(b * b - 4 * a * c)) / (2 * a)
