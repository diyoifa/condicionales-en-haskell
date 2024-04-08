module IF_THEN_ELSE_Part2 where

--(1)
--Ingrese tres números enteros y determine el mayor de los tres (haga uso de los
--operadores condicionales).
mayorDeTres :: (Int, Int, Int) -> Int
mayorDeTres (a, b, c) = if a > b 
                        then if a > c
                             then a
                             else c
                        else if b > c 
                             then b
                             else c


--(2)
--El pago de la cuota moderadora de una EPS, depende del valor del sueldo del
--afiliado, así: Si el sueldo es inferior a $900.000, la cuota es de $900; pero si
--es menor a $1.500.000 la cuota es de $1.300; en caso contrario es de $2.400
--pesos.

calcularCuotaModeradora :: Double -> Float
calcularCuotaModeradora sueldoAfiliado = if sueldoAfiliado < 900000
                                         then 900
                                         else if sueldoAfiliado < 1500000
                                              then 1300
                                              else 2400

--(3)
--Determine la categoría a la que pertenece un producto, de acuerdo a lo
--siguiente: Si el código del producto es menor o igual a 450, el producto es de
--categoría ‘A’, si el código es menor o igual a 700, la categoría es ‘B’; en caso
--contrario es de categoría ‘C’.

determinarCategoria :: Int -> Char
determinarCategoria codigoProducto = if codigoProducto <= 450
                                     then 'A'
                                     else if codigoProducto <= 700
                                          then 'B'
                                          else 'C'

--(4)
--El código de un estudiante consta de 5 cifras, la sede en la que estudia se
--determina por el último dígito de su código, de la siguiente manera:
--Si el código termina en una cifra entre 1 y 3, el estudiante pertenece a la sede
-- “Villa del Rosario”, si la cifra está entre 5 y 6, el estudiante pertenece a la sede
-- “Cúcuta”; y si la cifra está entre 7 y 9 (incluido el cero) a la sede “Pamplona”.

obtenerUltimoDigito :: Int -> Int
obtenerUltimoDigito codigo = codigo `mod` 10

verificarSede :: Int -> String
verificarSede codigo = if (obtenerUltimoDigito (codigo) == 0 )
                       then "Pamplona"
                       else if (obtenerUltimoDigito (codigo) < 3 )
                            then "Villa del Rosario"
                            else if (obtenerUltimoDigito (codigo) < 6 )
                                 then "Cucuta"
                                 else if (obtenerUltimoDigito (codigo) < 9 )
                                      then "Pamplona"
                                      else "Not found"

--(5)
--Dadas las 3 notas de parciales de un estudiante en la asignatura de Cálculo:
--parcial1(35%), parcial2(35%), parcial3(30%), calcular su nota definitiva.
--Para aprobar la asignatura se requiere una nota igual o superior a 3.0. En el
--caso, de que la nota del estudiante esté entre 2.0 y 2.9, el estudiante puede
--habilitar. Determine si el estudiante aprueba, reprueba o debe habilitar la
--asignatura.

calcularNotaDefinitiva :: (Float, Float, Float) -> Float
calcularNotaDefinitiva (parcial1, parcial2, parcial3) = parcial1 * 0.35 + parcial2 * 0.35 + parcial3 * 0.3

evaluarAprobacion :: (Float, Float, Float) -> String
evaluarAprobacion (parcial1, parcial2, parcial3) = if calcularNotaDefinitiva(parcial1, parcial2, parcial3) >= 3.0
                                                   then "aprueba"
                                                   else if calcularNotaDefinitiva(parcial1, parcial2, parcial3) < 2.0
                                                        then "reprueba"
                                                        else "habilita"
