module Main where

import Mundo
import Graphics
import qualified Conway

colorCell :: Conway.EstadoCelda -> PatchColor
colorCell Conway.Viva = red
colorCell Conway.Muerta  = black

main :: IO ()
main = do
	putStrLn "Comenzando simulación"
	mundo <- Conway.randomMundo 200 200
	doGraphics colorCell (evolve Conway.evolveCell) mundo
	putStrLn "Simulación Finalizada"
	return ()

