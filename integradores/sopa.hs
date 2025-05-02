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


masRepetido:
    tiene que recibir un tablero
    tiene que poder encontrar el elemento mas repetido en ese tablero

    depende de:

    recorrer las filas y columnas
    almacenar estado del elemento mas repetido y sus repeticiones

    termina si: 
    el tablero es vacio -> 0
    la busqueda finaliza -> devuelve mas repetido
-}

masRepetido :: Tablero -> Integer
masRepetido [] = 0
masRepetido tablero = buscarRepetido tablero 0 0

{-
funcion:
    recibe:
    devuelve:
    necesita:
    debe:
    depende:
    termina si:

buscar:
    recibe:
        tablero
        elemento mas repetido
        repeticiones

    devuelve 
        numero mas repetido
    
    necesita: 
        conocer cual es el maximo encontrado y su repeticiones maximas
    
    debe:
        1- contar un elemento en el tablero 

        2- comparar ese valor con el de la maxima maxima repeticion del elemento 
        que mas apariciones tenga de buscar en el tablero sin ese elemento

        SI
            encontre el mas repetido.
        sino
            el mas repetido esta dentro del tablero
            
    depende: 
        contar un elemento en el tablero
        borrar algun elemento del tablero

    termina si: 
        el tablero es vacio -> numero más repetido
        el tablero no es vacio -> sigue buscando


-}

-- buscar :: tablero -> masRepetidoParcial -> cantVeces -> masRepetido
buscarRepetido :: Tablero -> Integer -> Integer -> Integer
buscarRepetido [] n _ = n
buscarRepetido tablero mrp cv | contarPrimerElemento >= cv = buscarEnTableroReducido
                              | otherwise = buscarRepetido tableroReducido mrp cv
                    where primerElemento = head ( head tablero)
                          tableroReducido = limpiarListasVacias (borrarElementoTablero tablero primerElemento)
                          buscarEnTableroReducido = buscarRepetido tableroReducido primerElemento contarPrimerElemento
                          contarPrimerElemento = contarElementoTablero tablero primerElemento

limpiarListasVacias :: Tablero -> Tablero
limpiarListasVacias [] = []
limpiarListasVacias (lista:tablero) | lista == [] = limpiarListasVacias tablero
                                 | otherwise = lista:limpiarListasVacias tablero




-- buscarRepetido [ [2,3,6,3,10,13,2] ] , 0 , 0 -> 2 > 0 = buscarRepetido [[3,6,3,10,13]], 2, 2
-- buscarRepetido [[3,6,3,10,13]], 2, 2 -> 2 >= 2 = buscarRepetido [[6,10,13]], 3, 2
-- buscarREpetido [[6,10,13]], 3, 2 -> 2 >= 1 != buscarRepetido 

{-
funcion:
    recibe:
    devuelve:
    necesita:
    debe:
    depende:
    termina si:

borrarElementoTablero:
    recibe:
        tablero
        elemento a borrar
    
    devuelve:
        tablero sin elemento a borrar

    necesita: 
        recorrer las filas de un tablero y borrar elemento
    
    debe: 
        eliminar el elemento de cada fila
        juntar las filas sin el elemento
    
    depende:
        de eliminar elementos en filas
    
    termina si:
        el tablero es vacio -> []
        el tablero no es vacio -> sigue iterando las filas.
-}

borrarElementoTablero :: Tablero -> Integer -> Tablero
borrarElementoTablero [] _ = []
borrarElementoTablero (fila:tablero) elemento = (borrarElementoFila fila elemento):borrarElementoTablero tablero elemento 


{-


borrarElementoFila:
    recibe:
        Fila
        elemento
    devuelve:
        Fila sin elemento
    necesita:
        recorrer fila 
    debe: 
       comparar un elemento X de la fila con el elemento a borrar. 
    depende:
        nada
    termina si:
        fila es vacio -> []
        fila no es vacio  -> compara el siguiente
-}

borrarElementoFila :: Fila -> Integer -> Fila
borrarElementoFila [] _ = []
borrarElementoFila (x:fila) elemento | x == elemento = borrarElementoFila fila elemento
                                     | otherwise = x:(borrarElementoFila fila elemento)

{-

contarElementoTablero:
    recibe:
        tablero
        elemento
    
    devuelve:
        cantidad de veces que aparece elemento en tablero

    debe:
        recorrer el tablero contando la aparicion de elemento en todas sus filas
    
    depende:
        de contar elementos en una fila. 

    termina si:
        el tablero es vacio -> 0
        recorre todas las filas -> devuelve la cantidad de veces que aparece elemento
-}

contarElementoTablero :: Tablero -> Integer -> Integer
contarElementoTablero [] _ = 0
contarElementoTablero (fila:tablero) elemento = contarElementoFila fila elemento + contarElementoTablero tablero elemento


{-
contarElementoFila
    recibe:
        fila
        elemento
    
    devuelve:
        cantidad de veces que aparece elemento en fila

    debe:
        tomar un elemento de la fila y contar sus apariciones. 
    
    depende:
        de contar el elemento -> puedo hacerlo. 

    termina si:
        la fila es vacia -> 0
        recorre todos los elementos -> devuelve la cantidad de veces que aparece elemento
-}

contarElementoFila :: Fila -> Integer -> Integer
contarElementoFila [] _ = 0
contarElementoFila (e:resto) elemento |  e == elemento = 1 + contarElementoFila resto elemento
                                     | otherwise = contarElementoFila resto elemento


{-


masRepetido :: Tablero -> Integer
masRepetido [] = 0
masRepetido (fila:tablero) | contarElementoTablero tableroCompleto primerElementoFila >= contarElementoTablero (borrarElementoTablero tableroCompleto primerElementoFila) (masRepetido tableroCompleto) = primerElementoFila
                           | otherwise = masRepetido tablero
                           where tableroCompleto = fila:tablero
                                 primerElementoFila = head fila--}
{- 
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
                                where borrarElemento = borrarElementoFila fila elemento -}



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

getCamino :: Integer -> Camino
getCamino 1 = (1,1):(2,1):(2,2):(3,2):(4,2):(4,3):[]
getCamino 4 = (1,1):(1,2):[]

{-
Ejercicio 7. Implementar la funci´on valoresDeCamino :: Tablero ->Camino ->[Int]
problema valoresDeCamino (t: Tablero, c: Camino) : seq⟨Z⟩ {
requiere: {El tablero t es un tablero bien formado, es decir, la longitud de todas las filas es la misma, y tienen al
menos un elemento}
requiere: {Existe al menos una columna en el tablero t }
requiere: {El tablero t no es vac´ıo, todos los n´umeros del tablero son positivos, mayores estrictos a 0}
requiere: {El camino c es un camino v´alido, es decir, secuencia de posiciones adyacentes en la que solo es posible
desplazarse hacia la posici´on de la derecha o hacia abajo y todas las posiciones est´an dentro de los limites del tablero
t}
asegura: {res es igual a la secuencia de n´umeros que est´an en el camino c, ordenados de la misma forma que aparecen
las posiciones correspondientes en el camino.}
}
 -}

 {-
 funcion:
    recibe:
    devuelve:
    necesita:
    debe:
    depende:
    termina si:

valoresDeCamino:
    recibe: 
        Tablero (secuencia de filas)
        Camino (secuencia de posiciones)
    devuelve: 
        lista de valores del tablero encontrados en el camino en el orden señalado en camino

    necesita: 
        obtener una lista por posicion
        obtener un valor en la lista por posicion
        juntarlo al resto de los valores
    debe: 
        almacenar el resultado parcial
    depende: 
        funcion que devuelva uan fila determinada
        funcion que devuelva un valor de una fila
    termina si: 
        no hay más elementos -> []
        si hay más elementos -> sigue buscando con un set reducido.
 -}
 
valoresDeCamino :: Tablero -> Camino -> [Integer]
valoresDeCamino _ [] = []
valoresDeCamino tablero ((fila,columna):camino) = (obtenerValorListaAux (obtenerFilaAux tablero fila 1) columna 1):valoresDeCamino tablero camino

 {-
 obtenerValorLista:
    recibe:
        fila
        indice
    devuelve:
        valor en ese indice
    necesita:
        recorrer la lista y situarse en una posición. 
    debe: 
        saber en que elemento está parado. 
    depende:
        de una función que sepa en que posición está comparado con la pedida.
    termina si: 
        no hay más elementos -> []
        si hay más elementos -> sigue buscando con un set reducido. 
 -}

obtenerValorLista :: Fila -> Integer -> Integer
obtenerValorLista [] _ = 0
obtenerValorLista fila posicion = obtenerValorListaAux fila posicion 1

 {-
 obtenerValorListaAux:
    recibe:
        fila
        indiceVisto (empieza de 1)
        indicePedido
    devuelve
        valor en ese indice
    necesita:
        recorre la lista hasta la posición pedida y cuando llega al indiceVisto devuelve el valor.
    debe:
        validar posición y devolver valor.
    termina si:
        indiceVisto es igual al indicePedido
        sino sigue iterando sumando 1 al indice visto.
 -}

obtenerValorListaAux :: Fila -> Integer -> Integer -> Integer
obtenerValorListaAux (elemento:fila) posicionPedida posicionVista | posicionVista == posicionPedida = elemento
                                                                 | otherwise = obtenerValorListaAux fila posicionPedida (posicionVista+1) 

obtenerFila :: Tablero -> Integer -> Fila
obtenerFila [] _ = []
obtenerFila tablero posicion = obtenerFilaAux tablero posicion 1

obtenerFilaAux :: Tablero -> Integer -> Integer -> Fila
obtenerFilaAux (fila:tablero) indicePedido indiceVisto | indiceVisto == indicePedido = fila
                                                       | otherwise = obtenerFilaAux tablero indicePedido (indiceVisto+1)
