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
maximo 

maximoFila :: Fila -> Integer
maximoFila [] = 0
maximoFila e:fila | e > maximoFila fila = e
                  | otherwise =  maximoFila fila
                  
 