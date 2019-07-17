module Graphics 
  (doGraphics
  ,PatchColor,red,green,blue,white,black) 
where

import Control.Monad
import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Mundo

type PatchColor = (GLfloat,GLfloat,GLfloat)

black = (0.0,0.0,0.0) :: PatchColor
red   = (1.0,0.0,0.0) :: PatchColor
green = (0.0,1.0,0.0) :: PatchColor
blue  = (0.0,0.0,1.0) :: PatchColor
white = (1.0,1.0,1.0) :: PatchColor

patch :: Loc -> PatchColor -> IO ()
patch (x,y) (r,g,b) = do
  color $ Color3 r g b
  rect (Vertex2 xf yf) (Vertex2 (xf+1) (yf+1))
  where xf = fromIntegral x :: GLfloat
        yf = fromIntegral y :: GLfloat

display :: (a -> PatchColor) -> IORef (Mundo a) -> IO ()
display colorf mundoRef = do
  mundo <- get mundoRef
  clear [ColorBuffer]
  let locs = locationsIn (limitesMundo mundo)
  forM_ locs $ \loc -> patch loc (colorf $ cellAt mundo loc)
  swapBuffers

idle :: IORef (Mundo a) -> (Mundo a -> Mundo a) -> IO ()
idle w evolve = do
  w $~ evolve
  postRedisplay Nothing
  
reshape :: Size -> IO ()
reshape (Size w h) = do
  let x = max w h
  let size = Size x x
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

doGraphics :: (a -> PatchColor) -> (Mundo a -> Mundo a) -> Mundo a -> IO ()
doGraphics colorf evolvef mundo = do 
  (progname,args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  window <- createWindow "Conway"
  windowSize $= Size 500 500
  let ((x1,y1),(x2,y2)) = limitesMundo mundo
  ortho2D (fromIntegral x1) (fromIntegral x2) 
          (fromIntegral y1) (fromIntegral y2)
  mundoRef <- newIORef mundo
  displayCallback $= display colorf mundoRef
  reshapeCallback $= Just reshape
  idleCallback $= Just (idle mundoRef evolvef)
  mainLoop
