module Conway where

import Control.Monad (replicateM)
import System.Random
import Mundo (Mundo,Loc)
import qualified Mundo as W

-- Generic utility functions
  
count :: Eq a => a -> [a] -> Int
count x xs = length (filter (x==) xs)

-- Game of Life

data EstadoCelda = Viva | Muerta
  deriving (Eq,Show)

randomCell :: IO EstadoCelda
randomCell = do
  x <- randomIO
  return (if x then Viva else Muerta)
  
randomMundo :: Int -> Int -> IO (Mundo EstadoCelda)
randomMundo width height = do
  states <- replicateM (width * height) randomCell
  let bounds = ((0,0),(width-1,height-1))
  return $ W.fromList bounds W.Toroidal states

vecinasVivas :: Mundo EstadoCelda -> Loc -> Int
vecinasVivas w x = count Viva neighborStates
  where neighborStates = map cellAt (neighbors x)
        neighbors = W.vecinos W.mooreNeighbors w
        cellAt = W.cellAt w

transicion :: EstadoCelda -> Int -> EstadoCelda
transicion Muerta  n | n == 3           = Viva
transicion Viva n | n == 2 || n == 3 = Viva
transicion _ _                        = Muerta

evolveCell :: Mundo EstadoCelda -> Loc -> EstadoCelda -> EstadoCelda
evolveCell w x s = transicion s (vecinasVivas w x)

renderCell :: EstadoCelda -> Char
renderCell Viva = '@'
renderCell Muerta  = ' '
