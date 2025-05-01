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





-- TESTING --          
getFila :: Integer -> Fila
getFila 1 = [2,3,6,10,33,2]
getFila 2 = [2,1,6,10,33,2]
getFila 3 = [2,23,6,10,38,2]
getFila 4 = [2,3,6,10,13,2]

getTablero :: Integer -> Tablero
getTablero 1 = (getFila 1):(getFila 2):(getFila 3):(getFila 4):[]