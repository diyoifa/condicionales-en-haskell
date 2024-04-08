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

calcularMediano (x, y, z) | x > y && x < z = x 
                          | y > x && y < z = y
                          | otherwise = z 