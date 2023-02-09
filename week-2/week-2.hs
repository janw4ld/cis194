{-# LANGUAGE OverloadedStrings #-}
import           CodeWorld


fromTile :: Tile -> Picture
data Tile = Wall | Ground | Storage | Box | Blank
box, wall, ground, storage :: Picture

fromTile tile = case tile of
  Wall    -> wall
  Ground  -> ground
  Storage -> storage
  Box     -> box
  _       -> blank

box = colored brown (solidRectangle 1 1)
wall = colored grey (solidRectangle 1 1)
ground = colored yellow (solidRectangle 1 1)
storage = colored black (solidCircle 0.3) & ground


data Coords = C Integer Integer
{-
(??=) :: Maybe a-> a-> a
Just a ??= _ = a
Nothing ??= b = b
 -}
travEdge :: (Integer -> Picture) -> Picture
travEdge fn = go 10 -- (n??=10)
  where
    go :: Integer -> Picture
    go (-10) = fn (-10)
    go n     = fn n & go (n-1)

atCoords :: Picture -> Coords -> Picture
atCoords pic (C x y) = translated (fromIntegral x) (fromIntegral y) pic

drawTileAt :: Coords -> Picture
drawTileAt c = atCoords (fromTile (maze c)) c
{-
drawRow :: Integer -> Picture
drawRow y = travEdge (\x -> drawTileAt (C x y))  --Nothing
 -}
theMaze :: Picture
theMaze = travEdge (\y -> travEdge (\x -> drawTileAt (C x y)))


exercise3 :: IO ()
exercise3 = drawingOf theMaze

maze :: Coords -> Tile
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

data Direction = R | U | L | D

initialCoord :: Coords
initialCoord = C 0 0

adjacentCoord :: Direction -> Coords -> Coords
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

handleEvent :: Event -> Coords -> Coords
handleEvent (KeyPress key) c
    | key == "Right" = adjacentCoord R c
    | key == "Up"    = adjacentCoord U c
    | key == "Left"  = adjacentCoord L c
    | key == "Down"  = adjacentCoord D c
handleEvent _ c = c

drawState :: Coords -> Picture
drawState = atCoords theMaze

main :: IO ()
main = activityOf initialCoord handleEvent drawState
