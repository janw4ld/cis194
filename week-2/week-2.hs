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



travEdge :: (Integer -> Picture) -> Picture
-- This only works because the maze is a square
travEdge fn = go 10 where
  go :: Integer -> Picture
  go (-10) = fn (-10)
  go n     = fn n & go (n-1)


data Coords = C Integer Integer

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
  | x ==  2    && y <= 0     = Wall
  | x ==  3    && y <= 0     = Storage
  | x >= -2    && y == 0     = Box
  | otherwise                = Ground


data Direction = R | U | L | D
data State = S Direction Coords

initialPose :: State
initialPose = S D (C (-3) 3)

adjacentCoords :: Direction -> Coords -> Coords
adjacentCoords direction (C x y) = case direction of
    R -> C (x+1)  y
    U -> C  x    (y+1)
    L -> C (x-1)  y
    D -> C  x    (y-1)

newPose :: Text -> Coords -> State
newPose key c = S direction (adjacentCoords direction c) where
  direction = case key of
    "Right" ->  R
    "Up"    ->  U
    "Left"  ->  L
    "Down"  ->  D
    -- _      ->  Nothing


handleEvent :: Event -> State -> State
-- handleEvent (KeyPress "Esc") _ = initialPose
handleEvent (KeyPress key) (S _ c) =
  unwrap (newPose key) c where
  unwrap :: (Coords -> State) -> Coords -> State
  unwrap go s = S ((\(S d _) -> d) (go c)) (attempt s) where
    attempt c
      | isOk (maze ((\(S _ x) -> x) (go c))) = (\(S _ x) -> x) (go c)
      | otherwise = c
    isOk tile = case tile of
      Ground  -> True
      Storage -> True
      _       -> False
handleEvent _ (S d c) = S d c


player :: State ->  Picture
player (S direction c) = atCoords
  (rotate direction (colored red (styledLettering Bold Monospace ">"))) c
  & drawMaze where
    rotate :: Direction -> Picture -> Picture
    rotate direction pic = case direction of
      U -> rotated (pi/2) pic
      L -> rotated pi pic
      D -> rotated (3*pi/2) pic
      R -> pic
      -- Nothing -> pic


main :: IO ()
main = activityOf initialPose handleEvent player
