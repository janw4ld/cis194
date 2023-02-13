{-# LANGUAGE OverloadedStrings #-}
import           CodeWorld
import           Data.Map.Lazy       (Map, fromList, member, (!))
import           Data.Maybe          (fromJust)
import           Data.Text.Internal  (Text)
import           GHC.Base            (BCO)
import           Language.Haskell.TH (Lit (IntegerL))

------------ lists ------------

data List a = Empty | Entry a (List a)

mapList :: (a -> b) -> List a -> List b
mapList _ Empty        = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

combine :: List Picture -> Picture
combine Empty        = blank
combine (Entry p ps) = p & combine ps

elemCoords :: List Coords -> Coords -> Bool
elemCoords Empty _        = False
elemCoords (Entry x cs) c = (x `eqCoords` c) || elemCoords cs c

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
eqCoords (C x1 y1) (C x2 y2) = x1==x2 && y1==y2

------------ the maze ------------

maze :: Coords -> Tile
maze (C x y)
  | abs x  >  4 || abs y  > 4 = Blank
  | abs x ==  4 || abs y == 4 = Wall
  | x     ==  2 && y     <= 0 = Wall
  | x     ==  3 && y     <= 0 = Storage
  | x     >= -2 && y     == 0 = Box
  | otherwise                 = Ground

noBoxMaze :: Coords -> Tile -- ??why do they want this sig
noBoxMaze c = if tile == Box then Ground else tile where tile = maze c

mazeWithBoxes :: List Coords -> Coords -> Tile
mazeWithBoxes (Entry c Empty) _ = noBoxMaze c
---TODO cleanup here plz
mazeWithBoxes cs c = if elemCoords initialBoxList c then Box else noBoxMaze c

------------ state ------------

data State = S Direction Coords (List Coords)
initialState :: State
initialState = S D (C (-3) 3) initialBoxList

initialBoxList :: List Coords
initialBoxList = findBoxes coordsList

---TODO create a function that can be used with travEdge
coordsList :: List Coords -- This is soooo ugly
coordsList = go n n where
  n = 10
  go :: Integer -> Integer -> List Coords
  go (-11) (-11) = Empty
  go (-11) y     = go n (y-1)
  go x y         = Entry (C x y) (go (x-1) y)

findBoxes :: List Coords -> List Coords
findBoxes Empty        = Empty
findBoxes (Entry c cs) = if maze c == Box
  then Entry c (findBoxes cs) else findBoxes cs

------------ event handling ------------

moveFromTo :: Coords -> Coords -> Coords -> Coords -- * CONFIRMED WORKS
moveFromTo from to tile = if from `eqCoords` tile then to else tile

handleEvent :: Event -> State -> State
-- handleEvent (KeyPress "Esc") _ = initialPose
handleEvent (KeyPress key) (S _ startC boxes) | key `member` dirMap = let
  (S d targetC _) = newPose (dirMap!key) startC Empty
  newPose d c = S d (adjacentCoords d c)

  finalC
    | isOk (maze targetC) = targetC
    | otherwise           = startC
  isOk tile = case tile of
    Ground  -> True
    Storage -> True
    Box     -> True
    _       -> False

  newBoxes = mapList (moveFromTo startC finalC) boxes
  in S d finalC newBoxes
handleEvent _ (S d c boxes) = S d c boxes

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

atCoords :: Picture -> Coords -> Picture
(@>) = atCoords
atCoords pic (C x y) = translated (fromIntegral x) (fromIntegral y) pic

drawTileAt :: (Coords->Tile) -> Coords -> Picture
drawTileAt fn c = fromTile (fn c) @> c

drawMaze :: Picture
drawMaze = travEdge (\x -> travEdge $ drawTileAt noBoxMaze . C x)

drawBoxes :: List Coords -> Picture
drawBoxes cs = combine (mapList (atCoords (fromTile Box)) cs)

player :: Picture
player = colored red (styledLettering Bold Monospace ">")

drawState :: State -> Picture
drawState (S d c boxes) = drawPlayer & drawBoxes boxes & drawMaze where
  drawPlayer = rotated theta player @> c
  theta = case d of
    R -> 0
    U -> pi/2
    L -> pi
    D -> 3*pi/2

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
