{-# LANGUAGE OverloadedStrings #-}
import           CodeWorld
import           Data.Map.Lazy      (Map, fromList, member, (!))
import           Data.Maybe         (fromJust)
import           Data.Text.Internal (Text)

------------ lists ------------

data List a = Empty | Entry a (List a)

mapList :: (a -> b) -> List a -> List b
mapList _ Empty        = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

combine :: List Picture -> Picture
combine Empty        = blank
combine (Entry p ps) = p & combine ps

------------ coordinates and directions ------------

data Coords = C Integer Integer

data Direction = R | U | L | D

dirMap :: Map Text Direction
dirMap = fromList[
  ("Right", R),
  ("Up"   , U),
  ("Left" , L),
  ("Down" , D)
  ]

adjacentCoords :: Direction -> Coords -> Coords
adjacentCoords d (C x y) = case d of
  R -> C (x+1)  y
  U -> C  x    (y+1)
  L -> C (x-1)  y
  D -> C  x    (y-1)

---TODO
eqCoords :: Coords -> Coords -> Bool
eqCoords = undefined

moveFromTo :: Coords -> Coords -> Coords -> Coords
moveFromTo = undefined

------------ the maze ------------

maze :: Coords -> Tile
maze (C x y)
  | abs x  >  4 || abs y  > 4 = Blank
  | abs x ==  4 || abs y == 4 = Wall
  | x     ==  2 && y     <= 0 = Wall
  | x     ==  3 && y     <= 0 = Storage
  | x     >= -2 && y     == 0 = Box
  | otherwise                 = Ground

noBoxMaze :: Coords -> Tile
noBoxMaze = undefined

mazeWithBoxes :: List Coords -> Coords -> Tile
mazeWithBoxes = undefined

------------ state ------------

data State = S Direction Coords
initialState :: State
initialState = S D (C (-3) 3)

initialBoxes :: List Coords
initialBoxes = undefined --TODO

newState :: Direction -> Coords -> State
newState d c = S d (adjacentCoords d c)

------------ event handling ------------

handleEvent :: Event -> State -> State
-- handleEvent (KeyPress "Esc") _ = initialPose
handleEvent (KeyPress key) (S _ startC) | key `member` dirMap = let
  (S d targetC) = newState (dirMap!key) startC
  isOk tile = case tile of
      Ground  -> True
      Storage -> True
      _       -> False
  finalC
    | isOk (maze targetC) = targetC
    | otherwise           = startC
  in S d finalC
handleEvent _ (S d c) = S d c

------------ drawing ------------

fromTile :: Tile -> Picture
data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq)
fromTile tile = case tile of
  Wall    -> colored grey (solidRectangle 1 1)
  Ground  -> colored yellow (solidRectangle 1 1)
  Storage -> colored black (solidCircle 0.3) & fromTile Ground
  Box     -> colored brown (solidRectangle 1 1)
  _       -> blank

travEdge :: (Integer -> Picture) -> Picture
-- This only works because the maze is a square (and i hate it for that)
travEdge fn = go 10 where
  go :: Integer -> Picture
  go (-10) = fn (-10)
  go n     = fn n & go (n-1)

drawTileAt :: Coords -> Picture
drawTileAt c = fromTile (maze c) @> c

drawMaze :: Picture
drawMaze = travEdge (\x -> travEdge $ drawTileAt . C x)

player :: State -> Picture
player (S d c) =
  rotated theta (colored red (styledLettering Bold Monospace ">")) @> c where
  theta = case d of
    R -> 0
    U -> pi/2
    L -> pi
    D -> 3*pi/2

atCoords :: Picture -> Coords -> Picture
(@>) = atCoords
atCoords pic (C x y) = translated (fromIntegral x) (fromIntegral y) pic

drawBoxes :: List Coords -> Picture
drawBoxes cs = combine (mapList (atCoords (fromTile Box)) cs)

drawState :: State -> Picture
drawState s = player s & drawMaze --TODO draw boxes

------------ The complete activity ------------

sokoban :: Activity State
sokoban = Activity initialState handleEvent drawState

------------ The general activity type ------------

data Activity world = Activity
  world
  (Event -> world -> world)
  (world -> Picture)

runActivity :: Activity s -> IO ()
runActivity (Activity initial handler draw)
  = activityOf initial handler draw

------------ resetable activities ------------

resetable :: Activity s -> Activity s
resetable (Activity initial handler draw) =
  Activity initial handler' draw where
  handler' (KeyPress "Esc") _ = initial
  handler' e s                = handler e s

------------ start screen ------------

startScreen :: Picture
startScreen = scaled 3 3 (lettering "Sokoban!")

data SSState world = StartScreen | Running world

withStartScreen :: Activity s  -> Activity (SSState s)
withStartScreen (Activity initial handler draw) =
  Activity initial' handler' draw' where
  initial' = StartScreen
  handler' (KeyPress " ") StartScreen = Running initial
  handler' _              StartScreen = StartScreen
  handler' e              (Running s) = Running (handler e s)
  draw' StartScreen = startScreen
  draw' (Running s) = draw s


------------ main ------------

main :: IO ()
main = runActivity sokoban
