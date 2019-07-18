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
	mundo <- Conway.randomMundo 300 300
	doGraphics colorCell (evolve Conway.evolucionarCelda) mundo
	putStrLn "Simulación Finalizada"
	return ()
