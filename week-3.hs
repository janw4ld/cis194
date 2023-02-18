{-# LANGUAGE OverloadedStrings #-}

import CodeWorld {- hiding ((&))
                 import CodeWorld qualified ((&)) -}

import Data.Map.Lazy (Map, fromList, member, (!))
import Data.Text (Text, pack)

------------ lists ------------

data List a = Empty | Entry a (List a)

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & combine ps

elemList :: Eq a => List a -> a -> Bool
elemList Empty _ = False
elemList (Entry x cs) c = (x == c) || elemList cs c

------------ meta ------------
class Merge a where
  merge :: a -> a -> a
  infixr 0 `merge`
instance (Merge Picture) where merge = (CodeWorld.&)
instance (Merge (List Coords)) where
  merge Empty cs' = cs'
  merge (Entry c cs) cs' = Entry c (merge cs cs')

------------ coordinates and directions ------------

data Coords = C Integer Integer
instance (Eq Coords) where
  (==) :: Coords -> Coords -> Bool
  C x1 y1 == C x2 y2 = x1 == x2 && y1 == y2

data Direction = R | U | L | D

dirMap :: Map Text Direction
dirMap =
  fromList
    [ ("Right", R)
    , ("Up", U)
    , ("Left", L)
    , ("Down", D)
    ]

adjacentCoords :: Direction -> Coords -> Coords
adjacentCoords d (C x y) = case d of
  R -> C (x + 1) y
  U -> C x (y + 1)
  L -> C (x - 1) y
  D -> C x (y - 1)

------------ the maze ------------

maze :: Coords -> Tile
maze (C x y)
  | abs x > 4 || abs y > 4 = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x == 2 && y <= 0 = Wall
  | x == 3 && y <= 0 = Storage
  | x >= -2 && y == 0 = Box
  | otherwise = Ground

noBoxMaze :: Coords -> Tile -- ??why do they want this sig
noBoxMaze c = if tile == Box then Ground else tile where tile = maze c

mazeWithBoxes :: List Coords -> Coords -> Tile
---TODO cleanup here plz
mazeWithBoxes (Entry c Empty) _ = noBoxMaze c
mazeWithBoxes cs c = if elemList cs c then Box else noBoxMaze c

------------ state ------------

data State = S Direction Coords (List Coords)
initialState :: State
initialState = S D (C (-3) 3) initialBoxList

initialBoxList :: List Coords
initialBoxList = findTiles Box coordsList

storageList :: List Coords
storageList = findTiles Storage coordsList

isWon :: List Coords -> List Coords -> Bool -- I know I ignored the requirements, this is much simpler
isWon (Entry c cs) xs = elemList xs c && isWon cs xs
isWon Empty _ = True

coordsList :: List Coords
coordsList = travEdge $
  \x -> travEdge $
    \y -> Entry (C x y) Empty

findTiles :: Tile -> List Coords -> List Coords
findTiles _ Empty = Empty
findTiles t (Entry c cs) =
  if maze c == t
    then Entry c (findTiles t cs)
    else findTiles t cs

------------ event handling ------------

moveFromTo :: Eq a => a -> a -> a -> a
moveFromTo c0 c1 c = if c0 == c then c1 else c

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) (S _ startC boxes)
  | key `member` dirMap =
      let
        (S d targetC _) = newPose (dirMap ! key) startC Empty
        newPose d c = S d (adjacentCoords d c)

        final s t
          | isOk (mazeWithBoxes boxes t) = t
          | otherwise = s
        isOk tile = case tile of
          Ground -> True
          Storage -> True
          -- Box     -> True
          _ -> False

        canMove =
          let
            targetTile = mazeWithBoxes boxes targetC
            adjTile = mazeWithBoxes boxes (adjacentCoords d targetC)
           in
            isOk targetTile
              || ( targetTile == Box
                    &&
                    -- trace ("moved box: " <> pack (show (isOk adjTile)))
                    isOk adjTile
                 )
        finalC = if canMove then targetC else startC

        newBoxes = mapList (moveFromTo targetC (adjacentCoords d finalC)) boxes
       in
        if isWon boxes storageList then S d startC boxes else S d finalC newBoxes
handleEvent _ (S d c boxes) = S d c boxes

------------ drawing ------------

fromTile :: Tile -> Picture
data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq)
fromTile tile = case tile of
  Wall -> colored grey (solidRectangle 1 1)
  Ground -> colored yellow (solidRectangle 1 1)
  Storage -> colored black (solidCircle 0.3) & fromTile Ground
  Box -> colored brown (solidRectangle 1 1)
  _ -> blank

travEdge :: forall a. Merge a => (Integer -> a) -> a
travEdge fn = go 10
 where
  go :: Merge a => Integer -> a
  go (-10) = fn (-10)
  go n = fn n `merge` go (n - 1)

drawMaze :: Picture
drawMaze = travEdge $
  \x -> travEdge $
    \y -> drawTileAt noBoxMaze (C x y)

atCoords :: Picture -> Coords -> Picture
(@>) = atCoords
atCoords pic (C x y) = translated (fromIntegral x) (fromIntegral y) pic

drawTileAt :: (Coords -> Tile) -> Coords -> Picture
drawTileAt fn c = fromTile (fn c) @> c

drawBoxes :: List Coords -> Picture
drawBoxes cs = combine (mapList (atCoords (fromTile Box)) cs)

player :: Picture
player = colored red (styledLettering Bold Monospace ">")

drawState :: State -> Picture
drawState (S d c boxes) =
  if isWon boxes storageList
    then
      scaled 3 3 (lettering "You won!")
        & lettering "press esc to restart"
        @> C 0 (-3)
    else drawPlayer & drawBoxes boxes & drawMaze
 where
  drawPlayer = rotated theta player @> c
  theta = case d of
    R -> 0
    U -> pi / 2
    L -> pi
    D -> 3 * pi / 2

------------ The complete activity ------------

sokoban :: Activity (SSState State)
sokoban = resetable $ withStartScreen $ Activity initialState handleEvent drawState

------------ The general activity type ------------

data Activity world
  = Activity
      world
      (Event -> world -> world)
      (world -> Picture)

runActivity :: Activity s -> IO ()
runActivity (Activity initial handler draw) =
  activityOf initial handler draw

------------ resetable activities ------------

resetable :: Activity s -> Activity s
resetable (Activity initial handler draw) =
  Activity initial handler' draw
 where
  handler' (KeyPress "Esc") _ = initial
  handler' e s = handler e s

------------ start screen ------------

startScreen :: Picture
startScreen =
  scaled 3 3 (lettering "Sokoban!")
    & lettering "press space to start"
    @> C 0 (-3)

data SSState world = StartScreen | Running world

withStartScreen :: Activity s -> Activity (SSState s)
withStartScreen (Activity initial handler draw) =
  Activity initial' handler' draw'
 where
  initial' = StartScreen
  handler' (KeyPress " ") StartScreen = Running initial
  handler' _ StartScreen = StartScreen
  handler' e (Running s) = Running (handler e s)
  draw' StartScreen = startScreen
  draw' (Running s) = draw s

------------ main ------------

main :: IO ()
main = runActivity sokoban
