
-- BonBoayage

-- 1.

data Reserva = Reserva {
    pasajeros :: [String],
    tramos :: [Tramo],
    agregados :: [Agregado],
    costoBase :: Float
}

data Tramo = Tramo {
    origen :: String,
    destino :: String,
    duracion :: Float
} deriving (Show, Eq)

type Agregado = Reserva -> Reserva

lunchCompleto :: Agregado
lunchCompleto reserva = reserva { costoBase = costoBase reserva * 1.15 }

menuEspecial :: String -> Agregado
menuEspecial descripcion reserva = reserva { costoBase = costoBase reserva + (fromIntegral (length descripcion) * 10.0) }

equipajeExtra :: Int -> Agregado
equipajeExtra cantidadValijas reserva = reserva { costoBase = costoBase reserva + (fromIntegral cantidadValijas * 200.0) }

otroUtensilio :: String -> Agregado
otroUtensilio _ reserva = reserva { costoBase = costoBase reserva * 1.01 }

polizon :: String -> Agregado
polizon personaNueva reserva = reserva { pasajeros = personaNueva:pasajeros reserva}

-- 2.

-- a)

reservaLarga :: Reserva
reservaLarga = Reserva {
    pasajeros = ["Tom Borenstyn", "Frank Gorek"],
    tramos = [
        Tramo { origen = "Buenos Aires", destino = "São Paulo", duracion = 6 },
        Tramo { origen = "São Paulo", destino = "Londres", duracion = 10 }
    ],
    agregados = [
        otroUtensilio "auriculares", 
        otroUtensilio "auriculares",
        otroUtensilio "snacks"
    ],
    costoBase = 45000
}

-- b)

reservaCorta :: Reserva
reservaCorta = Reserva {
    pasajeros = ["César Frere"],
    tramos = [
        Tramo { origen = "Buenos Aires", destino = "Chascomús", duracion = 0.25 } 
    ],
    agregados = [
        otroUtensilio "almohada",
        otroUtensilio "manta",
        otroUtensilio "champagne",
        otroUtensilio "champagne", 
        otroUtensilio "auriculares",
        otroUtensilio "snacks",
        lunchCompleto, 
        equipajeExtra 3
    ],
    costoBase = 50000.0
}

-- 3.

calcularCosto :: Reserva -> Float
calcularCosto reservaInicial = costoBase reservaFinal
  where
    reservaFinal = foldl (flip ($)) reservaInicial (agregados reservaInicial)

-- 4. 

-- a)

sumarMonto :: Float -> Reserva -> Reserva
sumarMonto monto reserva = reserva {costoBase = costoBase reserva + monto}

-- b) 

sumarAgregado :: Agregado -> Reserva -> Reserva
sumarAgregado agregado reserva = reserva {agregados = agregado:agregados reserva}

-- c)

sumarTramo :: Tramo -> Reserva -> Reserva
sumarTramo tramo reserva = reserva {tramos = tramo:tramos reserva}

-- 5.

precioTotal :: Reserva -> Float
precioTotal reserva = calcularCosto reserva

-- 6.

esLarga :: Reserva -> Bool
esLarga reserva = sumarDuraciones (tramos reserva) > 15.0

sumarDuraciones :: [Tramo] -> Float
sumarDuraciones tramos = sum $ map (\tramo -> duracion tramo) tramos 

-- 7.

nuevaEscala :: Tramo -> Float -> Reserva -> Reserva
nuevaEscala tramoNuevo costoExtra reservaActual = sumarMonto costoExtra (sumarTramo tramoNuevo reservaActual)

-- 8.

reservaBienConstruida :: Reserva -> Bool
reservaBienConstruida reserva = revisarTramos (tramos reserva)

revisarTramos :: [Tramo] -> Bool
revisarTramos [] = False
revisarTramos [_] = True
revisarTramos (t1 : t2 : ts)
    | destino t1 /= origen t2 = False 
    | otherwise = revisarTramos (t2 : ts)

-- 9.

-- a)

precioReservaLarga :: Float
precioReservaLarga = calcularCosto (sumarAgregado (otroUtensilio "manta") (sumarAgregado (otroUtensilio "snacks") reservaLarga))

-- b)

modReservaLarga :: Bool
modReservaLarga = reservaBienConstruida (
        nuevaEscala (Tramo { origen = "Barcelona", destino = "Roma", duracion = 2.0 }) 4000.0
        (nuevaEscala (Tramo { origen = "Miami", destino = "Barcelona", duracion = 11.0 }) 15000.0 reservaLarga)
    )