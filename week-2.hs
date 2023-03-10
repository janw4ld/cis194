{-# LANGUAGE OverloadedStrings #-}

import CodeWorld
import Data.Map.Lazy (Map, fromList, member, (!))
import Data.Maybe (fromJust)
import Data.Text.Internal (Text)

fromTile :: Tile -> Picture
data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq)

fromTile tile = case tile of
  Wall -> colored grey (solidRectangle 1 1)
  Ground -> colored yellow (solidRectangle 1 1)
  Storage -> colored black (solidCircle 0.3) & fromTile Ground
  Box -> colored brown (solidRectangle 1 1)
  _ -> blank

travEdge :: (Integer -> Picture) -> Picture
-- This only works because the maze is a square
travEdge fn = go 10
 where
  go :: Integer -> Picture
  go (-10) = fn (-10)
  go n = fn n & go (n - 1)

data Coords = C Integer Integer

atCoords :: Picture -> Coords -> Picture
(@>) = atCoords
atCoords pic (C x y) = translated (fromIntegral x) (fromIntegral y) pic

drawTileAt :: Coords -> Picture
drawTileAt c = fromTile (maze c) @> c

drawMaze :: Picture
drawMaze = travEdge (\x -> travEdge $ drawTileAt . C x)

maze :: Coords -> Tile
maze (C x y)
  | abs x > 4 || abs y > 4 = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x == 2 && y <= 0 = Wall
  | x == 3 && y <= 0 = Storage
  | x >= -2 && y == 0 = Box
  | otherwise = Ground

data Direction = R | U | L | D
dirMap :: Map Text Direction
dirMap =
  fromList
    [ ("Right", R)
    , ("Up", U)
    , ("Left", L)
    , ("Down", D)
    ]

data State = S Direction Coords
initialState :: State
initialState = S D (C (-3) 3)

adjacentCoords :: Direction -> Coords -> Coords
adjacentCoords d (C x y) = case d of
  R -> C (x + 1) y
  U -> C x (y + 1)
  L -> C (x - 1) y
  D -> C x (y - 1)

newState :: Direction -> Coords -> State
newState d c = S d (adjacentCoords d c)

handleEvent :: Event -> State -> State
-- handleEvent (KeyPress "Esc") _ = initialPose
handleEvent (KeyPress key) (S _ startC)
  | key `member` dirMap =
      let
        (S d targetC) = newState (dirMap ! key) startC
        isOk tile = case tile of
          Ground -> True
          Storage -> True
          _ -> False
        finalC
          | isOk (maze targetC) = targetC
          | otherwise = startC
       in
        S d finalC
handleEvent _ (S d c) = S d c

resetableActivityOf ::
  world ->
  (Event -> world -> world) ->
  (world -> Picture) ->
  IO ()
resetableActivityOf initial handler = activityOf initial handler'
 where
  handler' (KeyPress "Esc") _ = initial
  handler' e s = handler e s

player :: State -> Picture
player (S d c) =
  rotated theta (colored red (styledLettering Bold Monospace ">")) @> c
 where
  theta = case d of
    R -> 0
    U -> pi / 2
    L -> pi
    D -> 3 * pi / 2

main :: IO ()
main = resetableActivityOf initialState handleEvent (\s -> player s & drawMaze)
