sumaPotenciasAux :: (Integer,Integer) -> Integer
sumaPotenciasAux (q,1) = q
sumaPotenciasAux (q,x) = (q ^ x) * (sumaPotenciasAux (q,(x-1)))

sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q n m = (sumaPotenciasAux (q,n)) * (sumaPotenciasAux (q,m))
