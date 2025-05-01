

quitar :: (Eq t) => [t] -> t -> [t]
quitar [] _ = []
quitar (x:xs) n | x == n = xs 
                | otherwise = x : quitar xs n 

maximo :: [Integer] -> Integer
maximo [] = -1 
maximo (x:xs) | x > maximo (xs) = x
              | otherwise = maximo (xs)

-- Ejercicio 1
longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

ultimo :: (Eq t) => [t] -> t 
ultimo (x:xs) | longitud xs == 0 = x
              | otherwise = ultimo xs

principio :: (Eq t) => [t] -> [t]
principio (x:xs) | longitud xs == 0 = []
                 | otherwise = x:principio xs

reverso :: (Eq t) => [t] -> [t]
reverso [] = []
reverso (xs) = ultimo xs : reverso (principio xs)

-- Ejercicio 2

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece y (x:xs) | x == y = True
                    | otherwise = pertenece y xs

todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True
todosIguales xs | xs == reverso xs = todosIguales (principio xs)
                | otherwise = False







lockers =[(100,(False,"ZD39I")),(101,(True,"JAH3I")),(103,(True,"IQSA9")),(105,(True,"QOTSA")),(109,(False,"893JJ")),(110,(False,"99292"))]




ordenarCreciente :: [Integer] -> [Integer]
ordenarCreciente [] = []
ordenarCreciente xs =  ordenarCreciente (quitar xs max) ++ [max]
                    where max = maximo xs

ordenarDecreciente :: [Integer] -> [Integer]
ordenarDecreciente [] = []
ordenarDecreciente xs =  max:ordenarDecreciente (quitar xs max) 
                    where max = maximo xs

