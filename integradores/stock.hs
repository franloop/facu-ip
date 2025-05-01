module Stock where 

type Producto = [Char]
type Stock = (Producto, Integer)
type Precio = (Producto, Float)

getMercaderia :: Integer -> [Producto]
getMercaderia 1 = ["mesa", "silla", "clavo", "mesa"]

getListaStock :: Integer -> [Stock]
getListaStock 1 = [("mesa", 20),("silla", 2),("clavo", 3)]

getListaPrecio :: Integer -> [Precio]
getListaPrecio 1 = [("mesa", 5.5),("silla", 2.3),("clavo", 0.10)]
getListaPrecio 2 = [("mesa", 10.5),("silla", 2),("clavo", 0.25)]


borrarElemento :: (Eq t) => t -> [t] -> [t]
borrarElemento _ [] = []
borrarElemento e (x:lista) | e == x = borrarElemento e lista
                           | otherwise = x:borrarElemento e lista

contarElemento :: (Eq t) => t ->  [t] -> Integer
contarElemento _ [] = 0
contarElemento elemento (x:lista) | elemento == x = 1 + contarElemento elemento lista
                                  | otherwise = 0 + contarElemento elemento lista


{--
Ejercicio 1. Implementar la funci´on generarStock :: [String] ->[(String, Int)]
problema generarStock (mercader´ıa: seq⟨String⟩) : seq⟨String × Z⟩ {
requiere: {True}
asegura: { La longitud de res es igual a la cantidad de productos distintos que hay en mercader´ıa}
asegura: {Para cada producto que pertenece a mercader´ıa, existe un i tal que 0 ≤ i < |res| y res[i]0=producto y
res[i]1 es igual a la cantidad de veces que aparece producto en mercader´ıa}
}
 --}

generarStock :: [Producto] -> [Stock]
generarStock [] = []
generarStock (mercaderia:listaMercaderia) = (mercaderia, (contarElemento mercaderia listaMercaderia)+1):generarStock (borrarElemento mercaderia listaMercaderia)



{--
Ejercicio 2. Implementar la funci´on stockDeProducto :: [(String, Int))] ->String
problema stockDeProducto (stock: seq⟨String × Z⟩, producto: String ) : Z {
requiere: {No existen dos nombres de productos (primeras componentes) iguales en stock}
requiere: {Todas las cantidades (segundas componentes) de stock son mayores a cero}
asegura: {si no existe un i tal que 0 ≤ i < |stock| y producto = stock[i]0 entonces res es igual a 0 }
asegura: {si existe un i tal que 0 ≤ i < |stock| y producto = stock[i]0 entonces res es igual a stock[i]1 }
--}


stockDeProducto :: [Stock] -> Producto -> Integer
stockDeProducto [] _ = 0
stockDeProducto (stock:listaStock) producto | fst(stock) == producto = snd(stock)
                                            | otherwise = stockDeProducto listaStock producto


{--
Ejercicio 3. Implementar la funci´on dineroEnStock :: [(String, Int))] ->[(String, Float)] ->Float
problema dineroEnStock (stock: seq⟨String × Z⟩, precios: seq⟨String × R⟩ ) : R {
    requiere: {No existen dos nombres de productos (primeras componentes) iguales en stock}
    requiere: {No existen dos nombres de productos (primeras componentes) iguales en precios}
    requiere: {Todas las cantidades (segundas componentes) de stock son mayores a cero}
    requiere: {Todos los precios (segundas componentes) de precios son mayores a cero}
    requiere: {Todo producto de stock aparece en la lista de precios}
    asegura: {res es igual a la suma de los precios de todos los productos que est´an en stock multiplicado por la cantidad
    de cada producto que hay en stock}
}
Para resolver este ejercicio pueden utilizar la funci´on del Preludio de Haskell fromIntegral que dado un valor de tipo
Int devuelve su equivalente de tipo Float.
--}

dineroEnStock :: [Stock] -> [Precio] -> Float
dineroEnStock [] _ = 0
dineroEnStock (stock:listaStock) listaPrecio = precioProducto listaPrecio (fst stock)* fromIntegral(snd stock) + dineroEnStock listaStock listaPrecio


precioProducto :: [Precio] -> Producto -> Float
precioProducto [] _ = 0
precioProducto (precio:precios) producto | fst precio == producto = snd precio
                                         | otherwise = precioProducto precios producto

{--
Ejercicio 4. 
Implementar la funci´on aplicarOferta :: [(String, Int)] ->[(String, Float)] ->[(String,Float)]

problema aplicarOferta (stock: seq⟨String × Z⟩, precios: seq⟨String × R⟩ ) : seq⟨String × R⟩ {

    requiere: {No existen dos nombres de productos (primeras componentes) iguales en stock}
    requiere: {No existen dos nombres de productos (primeras componentes) iguales en precios}
    requiere: {Todas las cantidades (segundas componentes) de stock son mayores a cero}
    requiere: {Todos los precios (segundas componentes) de precios son mayores a cero}
    requiere: {Todo producto de stock aparece en la lista de precios}

    asegura: {|res| = |precios|}
    asegura: {Para todo 0 ≤ i < |precios|, si stockDeProducto(stock, precios[i]0) > 10, entonces res[i]0 = precios[i]0 y
    res[i]1 = precios[i]1∗ 0,80}
    asegura: {Para todo 0 ≤ i < |precios|, si stockDeProducto(stock, precios[i]0) ≤ 10, entonces res[i]0 = precios[i]0 y
    res[i]1 = precios[i]1 }
}

--}

aplicarOferta :: [Stock] -> [Precio] -> [Precio]
aplicarOferta _ [] = []
-- np = nombre_producto, pp = precio_producto
aplicarOferta listaStock ((np, pp):listaPrecios) |  stockEnOferta = (np, pp * 0.8): aplicarOferta listaStock listaPrecios
                                                 |  otherwise = (np, pp): aplicarOferta listaStock listaPrecios
                                            where stockEnOferta = stockDeProducto listaStock np > 10 
 
 