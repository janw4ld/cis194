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

initialPose :: State
initialPose = S (Just D) (C (-3) 3)

data State = S (Maybe Direction) Coords | X Direction Coords

adjacentCoords :: Direction -> Coords -> State
adjacentCoords direction (C x y) = case direction of
  R -> X direction (C (x+1)  y)
  U -> X direction (C  x    (y+1))
  L -> X direction (C (x-1)  y)
  D -> X direction (C  x    (y-1))

newPose :: Text -> Coords -> State
newPose key = adjacentCoords map
  where
    map = case key of
      "Right" ->  R
      "Up"    ->  U
      "Left"  ->  L
      "Down"  ->  D
      -- _      ->  Nothing


handleEvent :: Event -> State -> State
handleEvent (KeyPress key) (S _ c) = wrap c (newPose key c)
  where
    -- wrap :: Coords ->(Direction, Coords) -> (Maybe Direction, Coords)
    wrap c (X d newCoords) = case maze newCoords of
          Ground  -> S (Just d) newCoords
          Storage -> S (Just d) newCoords
          _       -> S Nothing c
handleEvent _ (S d c) = S d c


player :: State ->  Picture
player (S direction c) = atCoords
  (rotate direction (colored red (styledLettering Bold Monospace ">"))) c & drawMaze
  where
    rotate :: Maybe Direction -> Picture -> Picture
    rotate direction pic = case direction of
      Just U  -> rotated (pi/2) pic
      Just L  -> rotated pi pic
      Just D  -> rotated (3*pi/2) pic
      Just R  -> pic
      Nothing -> pic


main :: IO ()
main = activityOf initialPose handleEvent player
