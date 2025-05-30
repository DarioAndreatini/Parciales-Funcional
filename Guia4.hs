
-- 1.

sumarLista :: [Int] -> Int
sumarLista = sum

-- 2.
-- a.)
frecuenciaCardiaca :: [Int]
frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125]

promedio :: [Int] -> Int
promedio lista = div (sumarLista lista) (length lista)

-- b.)
minutoPosicion :: Int -> Int
minutoPosicion minuto
    | minuto < 10 = 0
    | otherwise   = div minuto 10

frecuenciaCardiacaMinuto :: Int -> [Int] -> Int
frecuenciaCardiacaMinuto minuto lista = lista !! minutoPosicion minuto

-- c.)

frecuenciaHastaMomento :: Int -> [a] -> [a]
frecuenciaHastaMomento minuto = take (1 + minutoPosicion minuto)

-- 3.

esCapicua :: (Eq a, Foldable t) => t [a] -> Bool
esCapicua lista
    | concat lista == reverse (concat lista) = True
    | otherwise                              = False

-- 4. 

-- a.)

type FranjaHoraria = (String, [Int])

elegirPorMayor :: (Ord a) => (a, b) -> (a, b) -> b
elegirPorMayor (val1, res1) (val2, res2)
  | val1 >= val2 = res1
  | otherwise    = res2

duracionLlamadas :: (FranjaHoraria, FranjaHoraria)
duracionLlamadas = (("horarioReducido",[20,10,25,15]),("horarioNormal",[10,5,8,2,9,10]))

sumarMinutosFranja :: FranjaHoraria -> Int
sumarMinutosFranja = sum . snd

cuandoHabloMasMinutos :: String
cuandoHabloMasMinutos = elegirPorMayor (sumarMinutosFranja (fst duracionLlamadas), fst (fst duracionLlamadas)) (sumarMinutosFranja (snd duracionLlamadas), fst (snd duracionLlamadas))

-- b.)

contarLlamadasFranja :: FranjaHoraria -> Int
contarLlamadasFranja = length . snd

cuandoHizoMasLlamadas :: String
cuandoHizoMasLlamadas = elegirPorMayor (contarLlamadasFranja (fst duracionLlamadas), fst (fst duracionLlamadas)) (contarLlamadasFranja (snd duracionLlamadas), fst (snd duracionLlamadas))


-- 5.

existsAny :: (a -> Bool) -> (a, a, a) -> Bool
existsAny funcion (a, b, c) = funcion a || funcion b || funcion c

-- 6.
mejor :: (Ord b) => (a -> b) -> (a -> b) -> a -> b
mejor f1 f2 val = max (f1 val) (f2 val)

-- 7.
aplicarPar :: (a -> b) -> (a, a) -> (b, b)
aplicarPar f (x, y) = (f x, f y)

-- 8.
parDeFns :: (a -> b) -> (a -> c) -> a -> (b, c)
parDeFns f1 f2 val = (f1 val, f2 val)

-- 9.
esMultiploDeAlguno :: Int -> [Int] -> Bool
esMultiploDeAlguno multiplo = any (\numero -> 0 == div multiplo numero)

-- 10.
promedios :: [[Int]] -> [Int]
promedios = map (\lista -> div (sum lista) (length lista))

-- 11.

-- 12.
mejoresNotas :: [[Int]] -> [Int]
mejoresNotas = map maximum

-- 13.
aprobo :: [Int] -> Bool
aprobo notas = minimum notas >= 6

-- 14.
aprobaron :: [[Int]] -> [[Int]]
aprobaron = filter (\notasAlumno -> aprobo notasAlumno)

-- 15.
divisores :: Int -> [Int]
divisores n = filter (\x -> n `mod` x == 0) [1..n]

-- 16.
exists :: (a -> Bool) -> [a] -> Bool
exists = any

-- 17.
hayAlgunNegativo :: [Int] -> Bool
hayAlgunNegativo = any (< 0)

-- 18.
aplicarFunciones :: [a -> b] -> a -> [b]
aplicarFunciones funciones valor = map (\f -> f valor) funciones

-- 19.
sumaF :: (Num b) => [a -> b] -> a -> b
sumaF funciones numero = sum (aplicarFunciones funciones numero)

-- 20.
subirHabilidad :: Int -> [Int] -> [Int]
subirHabilidad aumento = map (\habilidadActual -> min 12 (habilidadActual + aumento))

-- 21.
flimitada :: (Ord b, Num b) => (a -> b) -> a -> b
flimitada f n = max 0 (min 12 (f n))

-- a.
cambiarHabilidad :: (Int -> Int) -> [Int] -> [Int]
cambiarHabilidad f = map (flimitada f)

-- b.


-- 23.
primerosPares :: [Int] -> [Int]
primerosPares = takeWhile even

primerosDivisores :: Int -> [Int] -> [Int]
primerosDivisores n = takeWhile (\x -> n `mod` x == 0)

primerosNoDivisores :: Int -> [Int] -> [Int]
primerosNoDivisores n = takeWhile (\x -> n `mod` x /= 0)

-- 24.
huboMesMejorDe :: [Int] -> [Int] -> Int -> Bool
huboMesMejorDe ingresos egresos numero = any (> numero) (zipWith (-) ingresos egresos)

-- 25.
-- a.)
crecimientoAnual :: Int -> Int
crecimientoAnual edad
  | edad == 1 = 22
  | edad == 2 = 20
  | edad == 3 = 18
  | edad >= 1 && edad <= 9 = 24 - (edad * 2)
  | edad >= 10 && edad <= 15 = 4
  | edad >= 16 && edad <= 17 = 2
  | edad >= 18 && edad <= 19 = 1
  | otherwise = 0

-- b.)
crecimientoEntreEdades :: Int -> Int -> Int
crecimientoEntreEdades edadInicial edadFinal = sum (map crecimientoAnual [edadInicial .. edadFinal - 1])

-- c.)
alturasEnUnAnio :: Int -> [Int] -> [Int]
alturasEnUnAnio edadActual = map (+ (crecimientoAnual edadActual))

-- d.)
alturaEnEdades :: Int -> Int -> [Int] -> [Int]
alturaEnEdades alturaInicial edadInicial = map (\edadFutura -> alturaInicial + crecimientoEntreEdades edadInicial edadFutura)

-- 
sumarListaFoldl :: (Num a) => [a] -> a
sumarListaFoldl = foldl (+) 0

sumarListaFoldr :: (Num a) => [a] -> a
sumarListaFoldr = foldr (+) 0

-- 
productoriaFoldl :: (Num a) => [a] -> a
productoriaFoldl = foldl (*) 1

productoriaFoldr :: (Num a) => [a] -> a
productoriaFoldr = foldr (*) 1

dispersionConUnFoldr :: (Ord a, Num a) => [a] -> a
dispersionConUnFoldr [] = 0 
dispersionConUnFoldr (x:xs) = (\(minimo, maximo) -> maximo - minimo) (foldr (\elem (currentMin, currentMax) -> (min elem currentMin, max elem currentMax)) (x, x) xs)


