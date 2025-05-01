
{--
 factorial: 
    Con recursion definimos el caso general para reducirlo a un caso base a través de operaciónes que reduzcan el "contador" (de izq a derecha)
--}
factorial :: Int -> Int
factorial n | n == 0 = 1
            | n == 1 = 1
            | n > 0 = n + factorial (n - 1)



{--

    factorial: 
        retorna 0 si la entrada es 0
        retorna 1 si la entrada es 1
        para n>1 se ejecuta recursivamente con su anterior y el anterior al anterior.
--}


fibonacci :: Int -> Int 
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | n > 1 = fibonacci ( n - 1) + fibonacci ( n - 2)


{--
    parteEntera: Ingresa un float (decimal) devolvemos solo la parte entera.
    requiere: x >= 0
    asegura: resultado =< x < resultado + 1

    concepto: buscar reducir el caso complejo al caso base. 
              buscar la reducción 
--}

parteEntera :: Float -> Int
parteEntera x | 0 <= x && x < 1 = 0
              | otherwise = 1 + parteEntera (x - 1)

{-- Idea: Poder pasarle por parametro el paso con el que queremos reducir la busqueda, 
y si nos pasamos, reducir en 1 el paso
parteEnteraPaso :: Float -> Float -> Float  
parteEnteraPaso x y -- | -- x < 0 = (y-1) + parteEnteraPaso(x (y-1))
                    | x >= 0 && x < 1 = 0
                    | x - y < 0 = (y-1) + (parteEnteraPaso x (y-1))
                    | otherwise = y + (parteEnteraPaso x y)
--}


{--
    ienesimoDigito: n como entrada, i como posicion, busca el numero que está en n en la posición i. 
    requiere: n >= 0 e 1 < i =< cantidadDigitos(n)
    asegura: resultado = ((n div 10 ^ (cantidadDigitos(n)-i)) mod 10)    
--}

ienesimoDigito :: Int -> Int -> Int
ienesimoDigito n i | n > 0 && i >= 1 && i <= (cantidadDigitos(n)) = mod (div n (10^(cantidadDigitos n - i))) 10

--otherwise = mod div (n (10^(cantidadDigitos(n)-i))) 10

cantidadDigitos :: Int -> Int
cantidadDigitos n | div n 10 == 0 = 1
                  | otherwise = 1 + cantidadDigitos(div n 10)

{--
    esCapicua: determinar en base a un numero n natural si es capicua    
-}

--esCapicua :: Int -> Bool
--esCapicua n | 

sacarPrimeroYultimo :: Int -> Int
sacarPrimeroYultimo n | n > 0 = div (mod n (10^((cantidadDigitos n)-1))) 10


esDivisible :: Integer -> Integer ->  Bool
esDivisible dividendo divisor | dividendo < 0 = False
                              | (dividendo-divisor)== 0 = True
                              | otherwise = esDivisible (dividendo-divisor) divisor


{- 
esCapicua :: Int -> Bool 
esCapicua 0



 esCapicua: 

 La idea es verificar si el primer digito y el último son iguales. 
 Si lo son, seguir hasta quedarme con 1 o 0 digitos. 
 Casos base: 0 (numero con cantidad de numeros pares) -> Llegue acá quiere decir que todos son iguales
 Caso base 1: queda un solo digito, por lo cual los anteriores son iguales. 
 -}

{- sumaPotencias: lo salteamos, hacer en casa Ej 14-}

{-
    Ej 16: menorDivisor: Int -> Int
    requiere d E Naturales 
    asegura:  : d = n . k + r, r = 0, donde n es el menor n posible. 
-}

menorDivisor :: Int -> Int
menorDivisor 1 = 1 
menorDivisor n = buscarDivisor 2 n

buscarDivisor :: Int -> Int -> Int
buscarDivisor i n | ( mod n i == 0) = i
                  | otherwise = buscarDivisor (i+1) n


{-
buscarTodos los divisores: 
buscarDivisores :: Int -> [Int]
buscarDivisores n = buscarDivisor(2 n)

buscarDivisoresDesde :: Int -> Int -> Int
buscarDivisoresDesde i n | buscarDivisor ( i n ) == i = 
COMPLETAR
-}


{-
    esSumaInicialDePrimos: Espera Int devulve Bool. 
    requiere : n > = 0 
    asegura = res == true si solo si n es = SUMA k primos para algun m)
-}

{-
-}
esSumaInicialDePrimos :: Int -> Bool
esSumaInicialDePrimos n = esSumaInicialDePrimosDesde 1 n

esSumaInicialDePrimosDesde :: Int -> Int -> Bool
esSumaInicialDePrimosDesde i n | (sumaKPrimos i) > n = False
                               | (sumaKPrimos i) < n = esSumaInicialDePrimosDesde (i+1) n
                               | otherwise = True

sumaKPrimos :: Int -> Int
sumaKPrimos 1 = 2
sumaKPrimos k = nEsimoPrimo k + sumaKPrimos(k-1)

nEsimoPrimo :: Int -> Int
nEsimoPrimo 1 = 2
nEsimoPrimo k = siguientePrimo (nEsimoPrimo(k-1) + 1)

siguientePrimo :: Int -> Int
siguientePrimo n | esPrimo(n) = n
                 | otherwise = siguientePrimo (n+1)

{-
primo. numero que solo se divide por 1 y por si mismo. 
-}
esPrimo :: Int -> Bool
esPrimo n | menorDivisor n  == n = True
          | otherwise = False


