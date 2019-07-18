module Conway where

import Control.Monad (replicateM)
import System.Random
import Mundo (Mundo,Loc)
import qualified Mundo as W

-- Generic utility functions

count :: Eq a => a -> [a] -> Int
count x xs = length (filter (x==) xs)

-- Juego de la vida
data EstadoCelda = Viva | Muerta
  deriving (Eq,Show)

celdaRandom :: IO EstadoCelda
celdaRandom = do
  x <- randomIO
  return (if x then Viva else Muerta)

randomMundo :: Int -> Int -> IO (Mundo EstadoCelda)
randomMundo ancho alto = do
  celdas <- replicateM (ancho * alto) celdaRandom
  let limites = ((0,0),(ancho-1,alto-1))
  return $ W.fromList limites W.Toroidal celdas

vecinasVivas :: Mundo EstadoCelda -> Loc -> Int
vecinasVivas w x = count Viva vecinasAlrededor
  where vecinasAlrededor = map cellAt (vecinos x)
        vecinos = W.vecinos W.mooreNeighbors w
        cellAt = W.cellAt w

transicion :: EstadoCelda -> Int -> EstadoCelda
transicion Muerta  n | n == 3           = Viva
transicion Viva n | n == 2 || n == 3 = Viva
transicion _ _                        = Muerta

evolucionarCelda :: Mundo EstadoCelda -> Loc -> EstadoCelda -> EstadoCelda
evolucionarCelda w x s = transicion s (vecinasVivas w x)

renderCelda :: EstadoCelda -> Char
renderCelda Viva = '@'
renderCelda Muerta  = ' '
