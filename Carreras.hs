
-- Carreras 

-- 1. 
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Hoist not" #-}

data Auto = Auto {
    color :: String,
    velocidad :: Int,
    distancia :: Int
} deriving(Show, Eq)

type Carrera = [Auto]

-- a.)

estaCerca :: Auto -> Auto -> Bool
estaCerca auto1 auto2
    | sonDistintos && distanciaMenorA10 = True
    | otherwise                         = False
    where
        sonDistintos = auto1 /= auto2
        distanciaMenorA10 = distancia auto1 - distancia auto2 < 10

-- b.)

vaTranquilo :: Carrera -> Auto -> Bool
vaTranquilo carrera auto = noEstaCercaNadie auto carrera && vaPrimero auto carrera

noEstaCercaNadie :: Auto -> [Auto] -> Bool
noEstaCercaNadie auto = all (not. estaCerca auto)

vaPrimero :: Auto -> [Auto] -> Bool
vaPrimero auto autos = all ((> distancia auto) . distancia) (filter (/= auto) autos)

-- c.)

puesto :: Auto -> Carrera -> Int
puesto auto carrera = 1 + length (filter ((> distancia auto). distancia) carrera)

-- 2.

-- a.)
correr :: Int -> Auto -> Auto
correr = modificarDistancia

modificarDistancia :: Int -> Auto -> Auto
modificarDistancia numero auto = auto {distancia = distancia auto + (numero * velocidad auto) }

-- b.)

-- i.
modificarVelocidad :: (Int -> Int) -> Auto -> Auto
modificarVelocidad funcion auto = auto {velocidad = funcion (velocidad auto)}

-- ii.
bajarVelocidad :: Int -> Auto -> Auto
bajarVelocidad cantidad = modificarVelocidad (\velocidad -> max 0 (velocidad - cantidad))

-- 3. 

type PowerUp = Carrera -> Carrera

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

-- a.) 

terremoto :: Auto -> PowerUp
terremoto auto = afectarALosQueCumplen (estaCerca auto) (bajarVelocidad 50)

-- b.)

miguelitos :: Int -> Auto -> PowerUp
miguelitos cantidad auto = afectarALosQueCumplen ((< distancia auto) . distancia) (bajarVelocidad cantidad)

-- c.) 

jetPack :: Int -> Auto -> Carrera -> Carrera
jetPack tiempo auto = map aplicarJet
  where
    aplicarJet a
      | a == auto = (correr tiempo . modificarVelocidad (*2)) a { velocidad = velocidad a }
      | otherwise = a


aumentarVelocidad :: Int -> Auto -> Auto
aumentarVelocidad cantidad = modificarVelocidad (cantidad *)

-- 4.

-- a.)

simularCarrera :: Carrera -> [Carrera -> Carrera] -> [(Int, String)]
simularCarrera carrera eventos = map (\auto -> (puesto auto final, color auto)) final
  where final = foldl (flip ($)) carrera eventos

-- b.)

-- i.
correnTodos :: Int -> Carrera -> Carrera
correnTodos tiempo = map (correr tiempo)

-- ii.
usaPowerUp :: (Auto -> PowerUp) -> String -> PowerUp
usaPowerUp powerUp colorBuscado carrera = powerUp (head (filter ((== colorBuscado) . color) carrera)) carrera

-- c.)

carreraInicial :: Carrera
carreraInicial = [Auto "rojo" 120 0, Auto "blanco" 120 0, Auto "azul" 120 0, Auto "negro" 120 0]

simulacion :: [(Int, String)]
simulacion = simularCarrera carreraInicial
  [ correnTodos 30
  , usaPowerUp (jetPack 3) "azul"
  , usaPowerUp terremoto "blanco"
  , correnTodos 40
  , usaPowerUp (miguelitos 20) "blanco"
  , usaPowerUp (jetPack 6) "negro"
  , correnTodos 10
  ]

-- 5.
-- a.)
-- Se puede hacer gracias a la funcion de afectarALosQueCumplen con la condicion de que sea el color seleccionado

-- b.)
-- Ninguno podria terminar debido al patron all, ya que debe evaluar toda la lista. Al ser infinita no termina nunca.
