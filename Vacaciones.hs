
-- Vacaciones

data Turista = Turista {
    cansancio :: Int,
    stress :: Int,
    solo :: Bool,
    idiomas :: [Idioma]
}

type Idioma = String

-- 1.
-- a)
ana :: Turista
ana = Turista {cansancio = 0, stress = 21, solo = False, idiomas = ["español"]}

-- b)
beto :: Turista
beto = Turista {cansancio = 15, stress = 12, solo = True, idiomas = ["español"]}

cathi :: Turista
cathi = Turista {cansancio = 15, stress = 12, solo = True, idiomas = ["español"]}

-- 2.

reductorDeStress :: Int -> Turista -> Turista
reductorDeStress reduccion turista = turista {stress = max 0 (stress turista - reduccion)}

reductorDeCansancio :: Int -> Turista -> Turista
reductorDeCansancio reduccion turista = turista {cansancio = max 0 (cansancio turista - reduccion)}

-- a)

type Excurcion = Turista -> Turista

irALaPlaya :: Excurcion
irALaPlaya turista
    | solo turista = reductorDeCansancio 5 turista
    | otherwise    = reductorDeStress 1 turista

apreciarElemento :: [Char] -> Excurcion
apreciarElemento cosaApreciada = reductorDeCansancio (length cosaApreciada)

salirAHablar :: Idioma -> Excurcion
salirAHablar idiomaNuevo turista = turista {idiomas = idiomaNuevo:idiomas turista}

caminar :: Int -> Excurcion
caminar minutos = reductorDeCansancio (- (calcularIntensidad minutos)) . reductorDeStress (calcularIntensidad minutos)

calcularIntensidad :: Int -> Int
calcularIntensidad x = div x 4

paseoEnBarco :: String -> Excurcion
paseoEnBarco marea
    | marea == "Fuerte" = reductorDeStress (-6) . reductorDeCansancio (-10)
    | marea == "Moderada" = id
    | marea == "Tranquila" = caminar 10 . apreciarElemento "mar" . salirAHablar "Aleman"

-- otra forma
data Marea = Fuerte | Moderada | Tranquila

paseoEnBarco' :: Marea -> Excurcion
paseoEnBarco' Fuerte    = reductorDeStress (-6) . reductorDeCansancio (-10)
paseoEnBarco' Moderada  = id
paseoEnBarco' Tranquila = caminar 10 . apreciarElemento "mar" . salirAHablar "Aleman"


hacerExcursion :: Turista -> Excurcion -> Turista
hacerExcursion turista excursion = excursion turista

-- b)

deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun :: (Turista -> Int) -> Turista -> Excurcion -> Int
deltaExcursionSegun indice turista excursion = deltaSegun indice (hacerExcursion turista excursion) turista

-- c)

esEducativa :: Turista -> Excurcion -> Bool
esEducativa turista  = (>0) . deltaExcursionSegun (length . idiomas) turista

-- forma completa
esEducativa' :: Turista -> Excurcion -> Bool
esEducativa' turista excursion = deltaExcursionSegun (length . idiomas) turista excursion > 0

esDesestresante :: Turista -> Excurcion -> Bool
esDesestresante turista = (<= -3) . deltaExcursionSegun stress turista

-- 3.

type Paquete = [Excurcion]

completo :: Paquete
completo = [caminar 20, apreciarElemento "cascada", caminar 40, salirAHablar "malecquiano"]

ladoB :: Excurcion -> Paquete
ladoB excurcion = [paseoEnBarco "Tranquila", excurcion, caminar 120]

islaVecina :: String -> Paquete
islaVecina marea
    | marea == "Fuerte" = opcionA
    | otherwise         = opcionB
    where
        opcionA = [paseoEnBarco marea, apreciarElemento "lago", paseoEnBarco marea]
        opcionB = [paseoEnBarco marea, irALaPlaya , paseoEnBarco marea]

-- a)

realizarTour :: Turista -> Paquete -> Turista
realizarTour turista paquete = reductorDeStress (- (length paquete)) (foldl (flip ($)) turista paquete)

-- b) 

propuestaConvincente :: Turista -> [Paquete] -> Bool
propuestaConvincente turista = any (esConvincente turista)

esConvincente :: Turista -> Paquete -> Bool
esConvincente turista = any (cumpleExpectativas turista)

cumpleExpectativas :: Turista -> Excurcion -> Bool
cumpleExpectativas turista excursion = esDesestresante turista excursion && not (solo (hacerExcursion turista excursion))

-- c)

efectividadDeTour :: [Turista] -> Paquete -> Int
efectividadDeTour turistas paquete = sum $ map (`obtenerEspiritualidad` paquete) (filter (`esConvincente` paquete) turistas)

obtenerEspiritualidad :: Turista -> Paquete -> Int
obtenerEspiritualidad turista paquete = deltaSegun nivelDeEspiritualidad (realizarTour turista paquete) turista

nivelDeEspiritualidad :: Turista -> Int
nivelDeEspiritualidad turista = cansancio turista + stress turista

