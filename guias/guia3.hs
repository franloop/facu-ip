
-- Ejercicio 6
-- problema bisiesto (a˜no: Z) : Bool {
-- requiere: {True}
-- asegura: {res=false ↔ año no es multiplo de 4 o año es multiplo de 100 pero no de 400}
-- }

bisiesto :: Int -> Bool
bisiesto anio = esMultiplo anio 4 && (not (esMultiplo anio 100) || not (esMultiplo anio 400))


esMultiplo :: Int -> Int -> Bool
esMultiplo numero multiplicando = mod numero multiplicando == 0


--Ejercicio 7
-- distanciaManhattan:: (Float, Float, Float) -> (Float, Float, Float) -> Float
-- problema distanciaManhattan (p: R × R × R, q: R × R × R) : R {
-- requiere: {True}
-- asegura: {res = Zum (2 i=0) |pi − qi|}
-- }

type Cord = (Float, Float, Float)

distanciaManhattan :: Cord -> Cord -> Float
distanciaManhattan (x1, y1, z1) (x2, y2, z2) = abs (x1-x2) + abs (y1-y2) + abs (z1-z2)

--Ejercicio 8
-- problema comparar (a : Z, b : Z) : Z {
-- requiere: {T rue}
--  asegura: {(res = 1) ↔ (sumaUltimosDosDigitos(a) < sumaUltimosDosDigitos(b))}
--  asegura: {(res = −1)    ↔ (sumaUltimosDosDigitos(a) > sumaUltimosDosDigitos(b))}
--  asegura: {(res = 0) ↔ (sumaUltimosDosDigitos(a) = sumaUltimosDosDigitos(b))}
-- }
-- problema sumaUltimosDosDigitos (x : Z) : Z {
--  requiere: {T rue}
--  asegura: {res = (|x| mod 10) + ( |x| 10  mod 10)}
-- }


absoluto :: Int -> Int
absoluto x | x >= 0 = x
           | otherwise = - x

comparar :: Int -> Int -> Int 
comparar x y | sumaUltimosDosDigitos x < sumaUltimosDosDigitos y = 1
             | sumaUltimosDosDigitos x > sumaUltimosDosDigitos y = -1
             | otherwise = 0

sumaUltimosDosDigitos :: Int -> Int 
sumaUltimosDosDigitos x = mod (absoluto x) 10 + mod (div (absoluto x) 10) 10
--
--Ejercicio 9
--Especificar: 
-- problema f1 (x: R) : R 
--{
-- requiere True
-- asegura { res = 1 <-> n = 0 }
--
--
--}

f4 :: Float -> Float -> Float
f4 x y = ( x + y ) /2

f5 :: ( Float , Float ) -> Float
f5 (x , y ) = ( x + y ) /2

promedio2 x = f5 (x, 2)