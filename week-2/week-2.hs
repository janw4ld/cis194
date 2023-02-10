{-# LANGUAGE OverloadedStrings #-}
import           CodeWorld
import           Data.Maybe         (fromJust)
import           Data.Text.Internal (Text)


fromTile :: Tile -> Picture
data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq)

fromTile tile = case tile of
  Wall    -> colored grey (solidRectangle 1 1)
  Ground  -> colored yellow (solidRectangle 1 1)
  Storage -> colored black (solidCircle 0.3) & fromTile Ground
  Box     -> colored brown (solidRectangle 1 1)
  _       -> blank


data Coords = C Integer Integer

travEdge :: (Integer -> Picture) -> Picture
-- This only works because the maze is a square
travEdge fn = go 10
  where
    go :: Integer -> Picture
    go (-10) = fn (-10)
    go n     = fn n & go (n-1)

atCoords :: Picture -> Coords -> Picture
atCoords pic (C x y) = translated (fromIntegral x) (fromIntegral y) pic

drawTileAt :: Coords -> Picture
drawTileAt c = atCoords (fromTile (maze c)) c

drawMaze :: Picture
drawMaze = travEdge (\x -> travEdge $ drawTileAt . C x)

maze :: Coords -> Tile
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

data Direction = R | U | L | D

initialPose :: (Maybe Direction, Coords)
initialPose = (Nothing ,C (-3) 3)

adjacentCoords :: Direction -> Coords -> (Direction, Coords)
adjacentCoords direction (C x y) = case direction of
  R -> (direction, C (x+1)  y)
  U -> (direction, C  x    (y+1))
  L -> (direction, C (x-1)  y)
  D -> (direction, C  x    (y-1))

newPose :: Text -> Coords -> (Direction, Coords)
newPose key = adjacentCoords map
  where
    map = case key of
      "Right" ->  R
      "Up"    ->  U
      "Left"  ->  L
      "Down"  ->  D
      -- _      ->  Nothing

handleEvent :: Event -> (Maybe Direction, Coords) -> (Maybe Direction, Coords)
handleEvent (KeyPress key) (_,c) = wrap (newPose key c)
  where
    wrap :: (Direction, Coords) -> (Maybe Direction, Coords)
    wrap (d, newCoords)
      | maze newCoords == Ground || maze newCoords == Storage = (Just d, newCoords)
      | otherwise                                             = (Nothing, c)
handleEvent _ (_,c) = (Nothing, c)


player :: (Maybe Direction, Coords) ->  Picture
player (direction,c) = atCoords (rotate (colored red (styledLettering Bold Monospace ">"))) c & drawMaze
  where
    rotate :: Picture -> Picture
    rotate pic = case direction of
      Nothing -> pic
      Just R  -> pic
      Just U  -> rotated (pi/2) pic
      Just L  -> rotated pi pic
      Just D  -> rotated (3*pi/2) pic


main :: IO ()
main = activityOf initialPose handleEvent player
