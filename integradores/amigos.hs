{-
Ejercicio 9. Implementar la funci´on divisoresPropios :: Int ->[Int]
problema divisoresPropios (n: Z) : seq⟨Z⟩ {
requiere: {n > 0}
asegura: {res contiene a todos los divisores propios de n, ordenados de menor a mayor}
asegura: {res no tiene elementos repetidos}
asegura: {res no contiene a ning´un elemento que no sea un divisor propio de n}
}
-}


divisoresPropios :: Integer -> [Integer]
divisoresPropios n = divisoresPropiosHasta n (div n 2) 1

divisoresPropiosHasta :: Integer -> Integer -> Integer -> [Integer]
divisoresPropiosHasta n divisorMaximo divisorActual | divisorMaximo < divisorActual = []
                                                    | esActualDivisor == 0 = divisorActual:dividirSiguiente
                                                    | esActualDivisor /= 0 = dividirSiguiente
                                                    where dividirSiguiente = divisoresPropiosHasta n divisorMaximo (divisorActual+1)
                                                          esActualDivisor = mod n divisorActual


{-
dph 8 4 1 

4 < 1 = no
8 / 1 == 0 si -> 1:dph 8 4 2

dph 8 4 2
4 < 2 = no
8 / 2 == 0 Si -> 8:dph 8 4 3

dph 8 4 3
4 < 3 = no
8 / 3 == 0 No -> dph 8 4 -}


{- 
Ejercicio 10. Implementar la funci´on sonAmigos :: Int ->Int ->Bool
problema sonAmigos (n,m: Z) : Bool {
requiere: {n > 0}
requiere: {m > 0}
requiere: {m̸ = n}
asegura: {res = True ⇔ n y m son n´umeros amigos}
}
-}

sonAmigos :: Integer -> Integer -> Bool
sonAmigos n m | sumarLista (divisoresPropios n) == m && sumarLista (divisoresPropios m) == n = True 
              | otherwise = False

sumarLista :: [Integer] -> Integer
sumarLista [] = 0
sumarLista (x:lista) = x + sumarLista lista


{-
Ejercicio 11. Implementar la funci´on losPrimerosNPerfectos :: Int ->[Int]
problema losPrimerosNPerfectos (n: Z) : seq⟨Z⟩ {
requiere: {n > 0}
asegura: {|res| = n}
asegura: {res es la lista de los primeros n n´umeros perfectos, de menor a mayor}
}
Por cuestiones de tiempos de ejecuci´on, no les recomendamos que prueben este ejercicio con un n > 4.

Un n´umero natural es perfecto cuando la suma de sus divisores propios (n´umeros que lo dividen menores a ´el) es igual
al mismo n´umero. Por ejemplo, 6 es un n´umero perfecto porque la suma de sus divisores propios (1,2 y 3) es igual a 6
-}

losPrimerosNPerfectos :: Integer -> [Integer]
losPrimerosNPerfectos n = losPrimerosNPerfectosHasta n 1

losPrimerosNPerfectosHasta :: Integer -> Integer -> [Integer]
losPrimerosNPerfectosHasta 0 _ = []
losPrimerosNPerfectosHasta cantidad actual | sumarLista (divisoresPropios actual) == actual = actual:losPrimerosNPerfectosHasta (cantidad -1) (actual+1)
                                            | otherwise = losPrimerosNPerfectosHasta cantidad (actual+1)

{-
Ejercicio 12. Implementar la funci´on listaDeAmigos :: [Int] ->[(Int,Int)]
problema listaDeAmigos (lista: seq⟨Z⟩) : seq⟨Z × Z⟩ {
requiere: {Todos los n´umeros de lista son mayores a 0}
requiere: {Todos los n´umeros de lista son distintos}
asegura: {res es una lista de tuplas sin repetidos, que contiene a todos los pares de n´umeros que pertenecen a lista
y son amigos entre s´ı}
asegura: {|res| es igual a la cantidad de pares de n´umeros amigos que hay en lista.}
}
-}
listaDeAmigos :: [Integer] -> [(Integer, Integer)]
listaDeAmigos [] = []
listaDeAmigos (elemento:lista) = (listaDeAmigosDeElemento elemento lista) ++ listaDeAmigos(lista)

listaDeAmigosDeElemento :: Integer -> [Integer] -> [(Integer, Integer)]
listaDeAmigosDeElemento _ [] = []
listaDeAmigosDeElemento elemento (x:lista) | sonAmigos elemento x == True = (elemento, x):listaDeAmigosDeElemento elemento lista  
                                           | otherwise = listaDeAmigosDeElemento elemento lista
