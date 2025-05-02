type Fila = [Integer]
type Tablero = [Fila]
type Posicion = (Integer, Integer)
type Camino = [Posicion]

{-- 

Ejercicio 5. Implementar la funci´on maximo :: Tablero ->Int
problema maximo (t: Tablero) : Z {
requiere: {El tablero t es un tablero bien formado, es decir, la longitud de todas las filas es la misma, y tienen al
menos un elemento}
requiere: {Existe al menos una columna en el tablero t }
requiere: {El tablero t no es vac´ıo, todos los n´umeros del tablero son positivos, mayor estricto a 0}
asegura: {res es igual al n´umero m´as grande del tablero t}
}

--}

maximo :: Tablero -> Integer
maximo [] = 1
maximo (fila:tablero) | maximoFila fila > maximo tablero = maximoFila fila
                    | otherwise = maximo tablero


maximoFila :: Fila -> Integer
maximoFila [] = 0
maximoFila (e:fila) | e > maximoFila fila = e
                  | otherwise =  maximoFila fila

{-- 
Ejercicio 6. Implementar la funci´on masRepetido :: Tablero ->Int
problema masRepetido (t: Tablero) : Z {
requiere: {El tablero t es un tablero bien formado, es decir, la longitud de todas las filas es la misma, y tienen al
menos un elemento}
requiere: {Existe al menos una columna en el tablero t }
requiere: {El tablero t no es vac´ıo, todos los n´umeros del tablero son positivos, mayor estricto a 0}
asegura: {res es igual al n´umero que m´as veces aparece en un tablero t. Si hay empate devuelve cualquiera de ellos}
}

masRepetido :: Tablero -> Integer
masRepetido [] = 0
masRepetido (fila:tablero) | contarElementoTablero tableroCompleto primerElementoFila >= contarElementoTablero (borrarElementoTablero tableroCompleto primerElementoFila) (masRepetido tableroCompleto) = primerElementoFila
                           | otherwise = masRepetido tablero
                           where tableroCompleto = fila:tablero
                                 primerElementoFila = head fila--}
-- Si convierto toda la matriz en una fila, puedo buscar facilmente cual es el repetido. 
masRepetido :: Tablero -> Integer
masRepetido [] = 0
masRepetido tablero = masRepetidoFila (aplanarTablero tablero)

-- No era tan facil buscar el mas repetido en una fila.
masRepetidoFila :: Fila -> Integer
masRepetidoFila [] = 0
masRepetidoFila fila | contarElementoFila fila elemento >= contarElementoFila filaRestante (masRepetidoFila filaRestante) = elemento
                                |  otherwise = masRepetidoFila (borrarElementoFila fila elemento)
                                where filaRestante = borrarElementoFila fila elemento
                                      elemento = head fila

aplanarTablero :: Tablero -> [Integer]
aplanarTablero [] = []
aplanarTablero (fila:tablero) = fila ++ aplanarTablero tablero

contarElementoFila :: Fila -> Integer -> Integer
contarElementoFila [] _ = 0
contarElementoFila (e:fila) elemento | e == elemento = 1 + contarSiguiente
                                     | otherwise = contarSiguiente
                                where contarSiguiente = contarElementoFila fila elemento

borrarElementoFila :: Fila -> Integer -> Fila
borrarElementoFila [] _ = []
borrarElementoFila (e:fila) elemento | e == elemento = borrarElemento
                                     | otherwise = e:borrarElemento
                                where borrarElemento = borrarElementoFila fila elemento

{-- 
contarElementoTablero :: Tablero -> Integer -> Integer
contarElementoTablero [] _ = 0
contarElementoTablero (fila:tablero) elemento = contarElementoFila fila elemento + contarElementoTablero tablero elemento 

borrarElementoTablero :: Tablero -> Integer -> Tablero
borrarElementoTablero [] _ = []
borrarElementoTablero (fila:tablero) elemento = (borrarElementoFila fila elemento):borrarElementoTablero tablero elemento  
--}


-- TESTING --          
getFila :: Integer -> Fila
getFila 1 = [3,3,3,3,6,10,33,2 ,3, 6, 3]
getFila 2 = [2,1,6,2,10,33,3,2]
getFila 3 = [2,23,2,6,2,1,4,5,10,38,2]
getFila 4 = [2,3,6,3,10,13,2]

getTablero :: Integer -> Tablero
getTablero 1 = (getFila 1):(getFila 2):(getFila 3):(getFila 4):[]
getTablero 2 = (getFila 2):(getFila 3):(getFila 4):[]
getTablero 3 = (getFila 3):(getFila 4):[]
getTablero 4 = (getFila 4):[]
getTablero 5 = []