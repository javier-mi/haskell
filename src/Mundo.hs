module Mundo where

import Data.Array

-- Utility functions

chunk :: Int -> [a] -> [[a]]
chunk _ [] = [[]]
chunk n xs = ys : chunk n zs
  where (ys,zs) = splitAt n xs
  
-- Mundo

type Loc = (Int,Int)
type Offset = Loc
type Rect = (Loc,Loc)

data Forma = Plana | Toroidal
  deriving (Eq,Show)

data Mundo a = Mundo {
  limitesMundo :: Rect,
  formaMundo  :: Forma,
  celdasMundo  :: Array Loc a
} deriving Eq

instance Show a => Show (Mundo a) where
  show = render (head . show)

instance Functor Mundo where
  fmap f (Mundo r shp cells) = Mundo r shp (fmap f cells)

world :: Rect -> Forma -> (Loc -> a) -> Mundo a
world limites forma f = Mundo limites forma celdas
  where locs = locationsIn limites
        states = map f locs
        celdas = array limites (zip locs states)

fromList :: Rect -> Forma -> [a] -> Mundo a
fromList limites forma xs = Mundo limites forma celdas
  where locs = locationsIn limites
        celdas = array limites (zip locs xs)

cells :: Mundo a -> [a]
cells w = map ((celdasMundo w)!) (locationsIn (limitesMundo w))

cellAt :: Mundo a -> Loc -> a
cellAt (Mundo _ _ cels) x = cels ! x

inside :: Mundo a -> Loc -> Bool
inside w = inRange (limitesMundo w)

locationsIn :: Rect -> [Loc]
locationsIn ((x1,y1),(x2,y2)) = [(x,y) | y <- [y1..y2], x <- [x1..x2]]

wrap :: Mundo a -> Loc -> Loc
wrap world (x,y) = 
  case formaMundo world of
    Plana -> (x,y)
    Toroidal -> 
      let 
        ((x1,y1),(x2,y2)) = limitesMundo world
        w = x2 - x1
        h = y2 - y1
      in
        ((x `mod` w) + x1,(y `mod` h) + y1)

vecinos :: [Offset] -> Mundo a -> Loc -> [Loc]
vecinos ds w (x,y) = filter (inside w) neighborLocs
  where neighborLocs = map (\(dx,dy) -> wrap' (x+dx,y+dy)) ds
        wrap' = wrap w

cardinalNeighbors :: [Offset]
cardinalNeighbors = [(0,-1),(-1, 0),(1, 0),(0, 1)]

mooreNeighbors :: [Offset]
mooreNeighbors = [(-1,-1),(0,-1),(1,-1),
                  (-1, 0),       (1, 0),
                  (-1, 1),(0, 1),(1, 1)]

render :: (a -> Char) -> Mundo a -> String
render f w = unlines $ chunk n $ fmap f $ cells w
  where ((x1,y1),(x2,y2)) = limitesMundo w
        n = (x2 - x1) + 1

evolve :: (Mundo a -> Loc -> a -> a) -> Mundo a -> Mundo a
evolve f w = Mundo limites forma celdas'
  where limites = limitesMundo w
        forma = formaMundo w
        celdas = celdasMundo w
        grid = locationsIn limites
        celdas' = array limites [(x,f w x (celdas!x)) | x <- grid]
