
-- Transformers


data Autobot = Robot String (Int,Int,Int) ((Int,Int,Int) -> (Int,Int)) | Vehiculo String (Int,Int) deriving (Eq)

optimus = Robot "Optimus Prime" (20,20,10) optimusTransformacion
optimusTransformacion (_,v,r) = (v * 5, r * 2)

jazz = Robot "Jazz" (8,35,3) jazzTransformacion
jazzTransformacion (_,v,r) = (v * 6, r * 3)

wheeljack = Robot "Wheeljack" (11,30,4) wheeljackTransformacion
wheeljackTransformacion (_,v,r) = (v * 4, r * 3)

bumblebee = Robot "Bumblebee" (10,33,5) bumblebeeTransformacion
bumblebeeTransformacion (_,v,r) = (v * 4, r * 2)

autobots = [ optimus, jazz, wheeljack, bumblebee ]


-- 1.

maximoSegun :: Ord b => (a -> a -> b) -> a -> a -> a
maximoSegun f x y
  | f y x > f x y = y
  | otherwise     = x


-- 2.

nombreAutobot :: Autobot -> String
nombreAutobot (Robot nombre _ _)  = nombre
nombreAutobot (Vehiculo nombre _) = nombre

fuerzaAutobot :: Autobot -> Int
fuerzaAutobot (Robot _ (fuerza, _, _) _) = fuerza
fuerzaAutobot (Vehiculo _ _)             = 0

velocidadAutobot :: Autobot -> Int
velocidadAutobot (Robot _ (_, velocidad, _) _)   = velocidad
velocidadAutobot (Vehiculo _ (velocidad, _))     = velocidad

resistenciaAutobot :: Autobot -> Int
resistenciaAutobot (Robot _ (_, _, resistencia) _) = resistencia
resistenciaAutobot (Vehiculo _ (_, resistencia))   = resistencia

-- 3.

transformar :: Autobot -> Autobot
transformar (Robot n (f,v,r) funcTransformacion) = Vehiculo n (funcTransformacion (f,v,r))
transformar (Vehiculo n (v,r))                   = Vehiculo n (v,r)

-- 4.

velocidadContra :: Autobot -> Autobot -> Int
velocidadContra autobot1 autobot2 = velocidadAutobot autobot1 - (max 0 (fuerzaAutobot autobot2 - resistenciaAutobot autobot1))


-- 5.

elMasRapido :: Autobot -> Autobot -> Autobot
elMasRapido autobot1 autobot2 = maximoSegun velocidadContra autobot1 autobot2

-- 6.

-- a)

domina :: Autobot -> Autobot -> Bool
domina unAutobot = (all ((==unAutobot).(uncurry elMasRapido)) . listaDeEnfrentamientos unAutobot)

listaDeEnfrentamientos :: Autobot -> Autobot -> [(Autobot, Autobot)]
listaDeEnfrentamientos unRobot otroRobot = [(unRobot, otroRobot), (unRobot, transformar otroRobot), (transformar unRobot, otroRobot), (transformar unRobot, transformar otroRobot)]

-- b)

losDominaATodos :: Autobot -> [Autobot] -> Bool
losDominaATodos robotMaestro otrosRobots = all (domina robotMaestro) otrosRobots

-- 7.

-- a)

quienesCumplen :: (Autobot -> Bool) -> [Autobot] -> [String]
quienesCumplen condicion = map nombreAutobot . filter condicion

-- b)

autobotsQueDominanYTerminanEnVocal :: [String]
autobotsQueDominanYTerminanEnVocal = quienesCumplen ( \autobot -> losDominaATodos autobot (filter (/= autobot) autobots) && terminaEnVocal autobot ) autobots

terminaEnVocal :: Autobot -> Bool
terminaEnVocal autobot = esVocal (last (nombreAutobot autobot))

esVocal :: Char -> Bool
esVocal c = elem c "aeiou"




