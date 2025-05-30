
-- Minigolfito

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart :: Jugador
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd :: Jugador
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa :: Jugador
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between :: (Eq a, Enum a) => a -> a -> a -> Bool
between n m x = elem x [n .. m]

maximoSegun :: (Foldable t, Ord a1) => (a2 -> a1) -> t a2 -> a2
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord a => (t -> a) -> t -> t -> t
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

-- 1.

-- a.)
type Palo = Habilidad -> Tiro

-- i.
putter :: Palo
putter (Habilidad _ precision) = UnTiro 10 (precision * 2) 0

-- ii.
madera :: Palo
madera (Habilidad _ precision) = UnTiro 100 (div precision 2) 5

-- iii.
hierros :: Int -> Palo
hierros n (Habilidad fuerza precision) = UnTiro (fuerza * n) (div precision n) (max 0 (n - 3))

-- b.)
palos :: [Palo]
palos = [putter, madera] ++ map hierros [1..10]

-- 2.
golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo (habilidad jugador)

-- 3.
type Obstaculo = Tiro -> Tiro

superarObstaculo :: (Tiro -> Bool) -> (Tiro -> Tiro) -> Obstaculo
superarObstaculo condicion efecto tiro
  | condicion tiro = efecto tiro
  | otherwise      = tiroDetenido tiro

tiroDetenido :: Tiro -> Tiro
tiroDetenido tiro = tiro {velocidad = 0, precision = 0, altura = 0}

-- a.) 
tunelConRampita :: Obstaculo
tunelConRampita = superarObstaculo condicionObstaculo efecto
  where
    condicionObstaculo tiro = precision tiro > 90 && altura tiro == 0
    efecto tiro = tiro { velocidad = velocidad tiro * 2, precision = 100, altura = 0 }

-- b.)
laguna :: Int -> Obstaculo
laguna largoLaguna = superarObstaculo condicionObstaculo efecto
    where
        condicionObstaculo tiro = velocidad tiro > 80 && between 1 5 (altura tiro)
        efecto tiro = tiro { altura = div (altura tiro)  largoLaguna }

-- c.)
hoyo :: Obstaculo
hoyo = superarObstaculo condicionObstaculo efecto
    where
        condicionObstaculo tiro = between 5 20 (velocidad tiro) && altura tiro == 0 && precision tiro > 95
        efecto = tiroDetenido

-- 4.

-- a.)
palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (\palo -> obstaculo (golpe jugador palo) /= tiroDetenido (golpe jugador palo)) palos

esTiroDetenido :: Tiro -> Bool
esTiroDetenido (UnTiro v p a) = v == 0 && p == 0 && a == 0


-- b.)
cuantosSupera :: Tiro -> [Obstaculo] -> Int
cuantosSupera _ [] = 0
cuantosSupera tiro (obtaculo : obtaculosRestantes)
  | tiroSiguiente == UnTiro 0 0 0 = 0
  | otherwise = 1 + cuantosSupera tiroSiguiente obtaculosRestantes
  where
    tiroSiguiente = obtaculo tiro


-- c.)
paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador obstaculos = maximoSegun (\palo -> cuantosSupera (golpe jugador palo) obstaculos) palos

-- 5.

