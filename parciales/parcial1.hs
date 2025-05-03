--Viva la democracia

type Formula = (String, String)

{-
1.1. Ejercicio 1 - Votos en Blanco
problema votosEnBlanco(f ormulas : seq < String × String >, votos : seq < Z >, cantT otalV otos : Z) : Z{
requiere : {f ormulasV alidas(f ormulas)}
requiere : {|f ormulas| = |votos|}
requiere : { Todos los elementos de votos son mayores o iguales que 0}
requiere : { La suma de todos los elementos de votos es menor o igual a cantT otalV otos}
asegura : {res es la cantidad de votos emitidos que no correspondieron a niguna de las f´ormulas que se presentaron }
}
-}

votosEnBlanco :: [Formula] -> [Integer] -> Integer -> Integer
votosEnBlanco [] [] _ = 0
votosEnBlanco formulas votos cantVotos = cantVotos - sumarLista (votos)

sumarLista :: (Num t) => [t] -> t
sumarLista [] = 0
sumarLista (n:ns) = n + sumarLista ns

{-
1.2. Ejercicio 2 - F´ormulas V´alidas
problema formulasValidas(f ormulas : seq < String × String >) : Bool{
requiere : {T rue}
asegura : {(res = true) ↔ f ormulas no contiene nombres repetidos, es decir que cada candidato est´a en una ´unica f´ormula (no
se puede ser candidato a presidente y a vicepresidente ni en la misma f´ormula ni en f´ormulas distintas) }
}
-}

formulasValidas :: [Formula] -> Bool
formulasValidas [] = True
formulasValidas formulas = not (hayRepetidos (aplanarFormulas formulas))

aplanarFormulas :: [Formula] -> [String]
aplanarFormulas [] = []
aplanarFormulas (formula:formulas) = (fst formula):(snd formula):aplanarFormulas formulas

hayRepetidos :: [String] -> Bool
hayRepetidos [] = False
hayRepetidos (elemento:lista) | elemento == obtenerElementoLista elemento lista = True
                            | otherwise = hayRepetidos lista

obtenerElementoLista :: String -> [String] -> String
obtenerElementoLista _ [] = ""
obtenerElementoLista x (elemento:lista) | x == elemento = x
                                        | otherwise = obtenerElementoLista x lista


{-
1.3. Ejercicio 3 - Porcentaje de Votos
problema porcentajeDeVotos(presidente : String, f ormulas : seq < String × String >, votos : seq < Z >) : R{
requiere : {La primera componente de algun elemento de f ormulas es presidente}
requiere : {f ormulasV alidas(f ormulas)}
requiere : {|f ormulas| = |votos|}
requiere : { Todos los elementos de votos son mayores o iguales que 0}
requiere : { Hay al menos un elemento de votos que es mayor estricto que 0}
asegura : {res es el porcentaje de votos que obtuvo la f´ormula encabezada por presidente sobre el total de votos afirmativos }
}
Para resolver este ejercicio pueden utilizar la siguiente funci´on que devuelve como Float la divisi´on entre dos n´umeros de tipo
Int:
division :: Int → Int → F loat
division a b = (f romIntegral a) / (f romIntegral b)
-}



{-
algoritmo:

encontrar los votos del parametro presidente
    funcion que busque una formula a partir de un string y devuelva su posicion
    funcion que a partir una posicion devuelva un voto
calcular el total de votos = votos afirmativos
    funcion que sume una lista (hecha)
calcular porcentajeDeVotos 
    por hacer

asegura : {res es el porcentaje de votos que obtuvo la f´ormula encabezada por presidente sobre el total de votos afirmativos }
}
-}

division :: Integer -> Integer -> Float
division a b = (fromIntegral a) / (fromIntegral b)

porcentajeDeVotos :: String -> [Formula] -> [Integer] -> Float
porcentajeDeVotos presidente formulas votos = porcentaje
                                        where totalVotosAfirmativos = sumarLista votos
                                              votoPresidente = obtenerVoto (localizarFormula presidente formulas 0) votos 0
                                              porcentaje = division (votoPresidente*100) totalVotosAfirmativos 

obtenerVoto :: Integer -> [Integer] -> Integer -> Integer
obtenerVoto _ [] _ = -1
obtenerVoto x (voto:votos) i | x == i = voto
                           | otherwise = obtenerVoto x votos (i+1)

localizarFormula :: String -> [Formula] -> Integer -> Integer
localizarFormula _ [] _ = -1 -- no deberia pasar
localizarFormula x (formula:formulas) i | x == fst formula = i
                                        | otherwise = localizarFormula x formulas (i+1)


{-
1.4. Ejercicio 4 - Pr´oximo Presidente
problema proximoPresidente(f ormulas : seq < String × String >, votos : seq < Z >) : String{
requiere : {La primera componente de algun elemento de f ormulas es presidente}
requiere : {f ormulasV alidas(f ormulas)}
requiere : {|f ormulas| = |votos|}
requiere : { Todos los elementos de votos son mayores o iguales que 0}
requiere : { Hay al menos un elemento de votos que es mayor estricto que 0}
requiere : {|f ormulas| > 0}
asegura : {res es el candidato a presidente de f ormulas m´as votado de acuerdo a los votos contabilizados en votos}
}
-}
{-
algoritmo: 
    por cada formula, obtener el presidente y pedir su porcentaje de votos. 
    si ese porcentaje es mayor al anterior, es el nuevo presidente
obtenerPresidente : recorrer la lista de formulas y obtener primer elemento
pedirporcentajes: funcion anterior, porcentajeDeVotos
comparar con el anterior: almacenar estado, necesito funcion auxiliar: 
    proximoPresidenteAux formulas votos presidenteActual porcentajeActual
-}
{-}
proximoPresidente :: [Formula] -> [Integer] -> String
proximoPresidente formulas votos = proximoPresidenteAux formulas votos "" 0

proximoPresidenteAux :: [Formula] -> [Integer] -> String -> Float -> String
proximoPresidenteAux [] [] presidente _ = presidente
proximoPresidenteAux (formula:formulas) (voto:votos) presidenteActual porcentajeActual 
    | porcentajePresidente >= porcentajeActual = proximoPresidenteAux formulas votos nombrePresidente porcentajePresidente
    | otherwise = proximoPresidenteAux formulas votos presidenteActual porcentajeActual
    where porcentajePresidente = porcentajeDeVotos nombrePresidente (formula:formulas) (voto:votos)
          nombrePresidente = fst formula

          -- Esto tiene un problema, estoy pasando una lista cada vez mas reducida, por lo cual los calculos de porcetanjes se realizan sobre listas cada vez mas chicas. 
-}
{-
algoritmo: 
    buscar el voto mas grande y devolver su posicion. 
    obtener la formula en esa posicion
    devolver el primer elemento.
-}

proximoPresidente :: [Formula] -> [Integer] -> String
proximoPresidente [] [] = ""
proximoPresidente formulas votos = fst( localizarFormulaPosicion formulas (obtenerPosicionVoto votos (maximo votos 0) 0) 0)

--obtenerVotoMaximo dado una lista, devuelve la posicion del voto maximo encontrado. 
obtenerPosicionVoto :: [Integer] -> Integer -> Integer -> Integer
obtenerPosicionVoto [] _ _ = -1
obtenerPosicionVoto (voto:votos) votoBuscado posicion | voto == votoBuscado = posicion
                                                      | otherwise = obtenerPosicionVoto votos votoBuscado (posicion+1)

maximo :: [Integer] -> Integer -> Integer
maximo [] n = n
maximo (x:xs) n | x > n = maximo xs x 
                | otherwise = maximo xs n

localizarFormulaPosicion :: [Formula] -> Integer -> Integer -> Formula
localizarFormulaPosicion [] _ _ = undefined -- No es un caso posible dado los requiere de |formulas| = |votos|
localizarFormulaPosicion (formula:formulas) posicionDeseada posicionActual 
    | posicionDeseada == posicionActual = formula                                                                       
    | otherwise = localizarFormulaPosicion formulas posicionDeseada (posicionActual+1)




getFormula :: Integer -> Formula
getFormula 1 = ("Nicolas K.", "Maria P.")
getFormula 2 = ("Juan M.", "Cristian H.")
getFormula 3 = ("Romina G.", "Carla M.")


getFormula 4 = ("Juan C.", "Rita D.")
getFormula 5 = ("Maria P.", "Rita E.")

getFormulas :: Integer -> [Formula]
getFormulas 1 = (getFormula 1):(getFormula 2):(getFormula 3):[]
getFormulas 2 = (getFormula 1):(getFormula 2):(getFormula 3):(getFormula 5):[]

getVotos :: Integer -> [Integer]
getVotos 1 = [11,5,12]
getVotos 2 = [10,2,5,1]
