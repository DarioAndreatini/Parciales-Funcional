
-- Caen los Mercados

-- 1.
data Accion = Accion {
    simbolo :: String,
    precios :: [Int]
} deriving (Show, Eq)

data Usuario = Usuario {
    efectivo :: Int,
    cartera :: [TituloAccion]
} deriving (Show, Eq)

data TituloAccion = TituloAccion {
    simboloTitulo :: String,
    cantidad :: Int,
    precioCompra :: Int
} deriving (Show, Eq)

type Mercado = [Accion]

-- 2.

-- a)
mapCondicional :: (a -> a) -> (a -> Bool) -> [a] -> [a]
mapCondicional transformacion condicion lista 
    | any ( \elemento -> condicion elemento) lista = map (\elemento -> transformacion elemento ) lista
    | otherwise                                    = id lista


-- b)
encontrar :: (a -> Bool) -> [a] -> a
encontrar condicion lista = head (filter ( \elemento -> condicion elemento) lista)


-- c)
cuantasTieneDe :: String -> Usuario -> Int -- suma las cantidades de los titulos filtrados que posee el usuario
cuantasTieneDe simbolo usuario = sum (map cantidad (listaFiltradaPorSimbolo simbolo (cartera usuario)))

listaFiltradaPorSimbolo :: String -> [TituloAccion] -> [TituloAccion] -- filtra los titulos que tiene en cartera con el mismo simbolo y lo guarda en una lista
listaFiltradaPorSimbolo simbolo cartera = filter (\titulo -> simboloTitulo titulo == simbolo) cartera

-- 3.

-- a.
nuevoPrecioAccion :: Accion -> Int -> Accion
nuevoPrecioAccion accion nuevoValor = accion { precios = nuevoValor : precios accion }

-- b.
nuevoPrecio :: [Accion] -> String -> Int -> [Accion]
nuevoPrecio mercado simboloAccion nuevoValor = map (aplicarActualizacion simboloAccion nuevoValor) mercado

aplicarActualizacion :: String -> Int -> Accion -> Accion
aplicarActualizacion simboloBuscado nuevoValor accion
    | simbolo accion == simboloBuscado = nuevoPrecioAccion accion nuevoValor
    | otherwise                        = accion

-- c.
precioActual :: Accion -> Int
precioActual accion = head (precios accion)

-- 4.

estadoActual :: Usuario -> Mercado -> Int
estadoActual usuario mercado = sum (map (ganancia mercado) (cartera usuario))

ganancia :: Mercado -> TituloAccion -> Int
ganancia mercado titulo = calcularDiferencia (fromIntegral (cantidad titulo)) (precioCompra titulo) (buscarPrecioDeSimbolo (simboloTitulo titulo) mercado)

calcularDiferencia :: Int -> Int -> Int -> Int
calcularDiferencia cantidad precioCompra precioActualTitulo = (cantidad * precioActualTitulo) - (cantidad * precioCompra)

buscarPrecioDeSimbolo :: String -> Mercado -> Int
buscarPrecioDeSimbolo simboloBuscado mercado = head (map precioActual (filter ((== simboloBuscado) . simbolo) mercado) ++ [0])

-- 5.

pagarDividendos :: [Usuario] -> String -> Int -> [Usuario]
pagarDividendos usuarios simbolo monto = map (actualizarEfectivo simbolo monto) usuarios

actualizarEfectivo :: String -> Int -> Usuario -> Usuario
actualizarEfectivo simbolo monto usuario = usuario { efectivo = efectivo usuario + (fromIntegral (cantidadAcciones simbolo (cartera usuario)) * monto) }

cantidadAcciones :: String -> [TituloAccion] -> Int
cantidadAcciones simboloBuscado = sum . map cantidad . filter ((== simboloBuscado) . simboloTitulo)

-- 6.

noLeVaBien :: Usuario -> Mercado -> Bool
noLeVaBien usuario mercado = (estadoActual usuario mercado) < (-50000)

sumarMilEfectivo :: Usuario -> Usuario
sumarMilEfectivo usuario = usuario { efectivo = efectivo usuario + 1000 }

rescateFinanciero :: Mercado -> [Usuario] -> [Usuario]
rescateFinanciero mercado = map (aplicarRescate mercado)

aplicarRescate :: Mercado -> Usuario -> Usuario
aplicarRescate mercado usuario
    | noLeVaBien usuario mercado = sumarMilEfectivo usuario
    | otherwise                  = usuario

-- 7.

venta :: Usuario -> Accion -> Int -> Usuario
venta usuario accionAVender cantidadVendida =
    usuario {
        cartera  = modificarCantidadTitulo (simbolo accionAVender) cantidadVendida (cartera usuario),
        efectivo = efectivo usuario + (precioActual accionAVender * cantidadVendida)
    }

modificarCantidadTitulo :: String -> Int -> [TituloAccion] -> [TituloAccion]
modificarCantidadTitulo simboloAVender cantidadVendida = map (modificarSiCoincide simboloAVender cantidadVendida)

modificarSiCoincide :: String -> Int -> TituloAccion -> TituloAccion
modificarSiCoincide simboloAVender cantidadVendida titulo
    | simboloTitulo titulo == simboloAVender = titulo { cantidad = cantidad titulo - cantidadVendida }
    | otherwise                              = titulo

-- 8.

-- a)

primerMedicion :: Accion -> Int
primerMedicion (Accion _ []) = 0 
primerMedicion (Accion _ preciosHistoricos) = last preciosHistoricos

porcentajeDeGanancia :: Accion -> Int
porcentajeDeGanancia accion
    | primerMedicion accion == 0 = 0
    | otherwise                  = (fromIntegral (precioActual accion) * 100) `div` fromIntegral (primerMedicion accion)

-- b) 

mayorGanancia :: Accion -> Accion -> Accion
mayorGanancia accion1 accion2
    | porcentajeDeGanancia accion1 >= porcentajeDeGanancia accion2 = accion1
    | otherwise                                                    = accion2

-- c)

laMejorAccion :: [Accion] -> Accion
laMejorAccion [] = error "La lista de acciones no puede estar vacía para encontrar la mejor acción." 
laMejorAccion [accion] = accion 
laMejorAccion (accion1 : accion2 : restoDeAcciones) = laMejorAccion (mayorGanancia accion1 accion2 : restoDeAcciones) 





