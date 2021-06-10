module Library where
import PdePreludat

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Number ,
  precisionJugador :: Number 
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Number ,
  precision :: Number ,
  altura :: Number 
} deriving (Eq, Show)

type Puntos = Number 

-- Funciones útiles
between n m x = x `elem` [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

--PUNTO 1) a)
type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro 10 ( 2 * precisionJugador habilidad) 0

madera :: Palo
madera habilidad = UnTiro 100 (precisionJugador habilidad / 2) 5

hierro :: Number -> Palo
hierro n habilidad = UnTiro (fuerzaJugador habilidad*n) (precisionJugador habilidad / n) (max 0 n-3) 

--PUNTO 1) b)
palos :: [Palo]
palos = [putter,madera] ++ map hierro [1..10]

--PUNTO 2)
golpe :: Palo -> Jugador -> Tiro
golpe palo = palo . habilidad

--PUNTO 3)
data Obstaculo = UnObstaculo {
    puedeSuperar :: Tiro -> Bool 
,   efectoLuegoDeSuperar :: Tiro -> Tiro
}
  
intentarSuperarObstaculo :: Obstaculo -> Tiro -> Tiro
intentarSuperarObstaculo obstaculo tiro
    | puedeSuperar obstaculo tiro = efectoLuegoDeSuperar obstaculo tiro
    | otherwise  = tiroDetenido

tunelConRampita :: Obstaculo
tunelConRampita = UnObstaculo superaTunelConRampita efectoTunelConRampita

laguna :: Obstaculo
laguna = UnObstaculo superaLaguna efectoLaguna

hoyo :: Obstaculo
hoyo = UnObstaculo superaHoyo efectoHoyo

superaTunelConRampita :: Tiro -> Bool
superaTunelConRampita = (...)

superaLaguna :: Tiro -> Bool 
superaLaguna = (...)

superaHoyo :: Tiro -> Bool 
superaHoyo = (...)


efectoTunelConRampita :: Tiro -> Tiro
efectoTunelConRampita = (...)

efectoLaguna :: Tiro -> Tiro
efectoLaguna = (...)

efectoHoyo :: Tiro -> Tiro
efectoHoyo = (...)

tiroDetenido :: Tiro
tiroDetenido = UnTiro 0 0 0