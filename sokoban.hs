{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Wimplicit-prelude #-}

import CodeWorld
import Prelude -- hiding (elem)

import Data.Map.Lazy (Map, fromList, member, (!))
import Data.Maybe (fromMaybe)
import Data.Text (Text)

------------ lists ------------

reduce' :: (b -> b -> b) -> (a -> b) -> [a] -> Maybe b -- it's wonky
reduce' _ _ [] = Nothing
reduce' select go (x : xs) = Just $ select' (go x) (reduce' select go xs)
 where
  select' a Nothing = a
  select' a (Just b) = select a b
{- 
elem :: Eq a => a -> [a] -> Bool
elem c cs = fromMaybe False $ reduce' (||) (== c) cs
 -}
subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = fromMaybe False $ reduce' (&&) (`elem` ys) xs

listLength :: [a] -> Integer
listLength xs = fromMaybe 0 (reduce' (+) (const 1) xs)

filterList :: (a -> Bool) -> [a] -> [a]
filterList _ [] = []
filterList isOk (c : cs)
  | isOk c = c : filterList isOk cs
  | otherwise = filterList isOk cs

nth :: [a] -> Integer -> a
{- nth [1,2,3,4] 0 -> 1
   nth [1,2,3,4] 2 -> 3 -}
nth [] c = error $ "out of bounds, input index is off by " ++ show (c + 1)
nth (c : _) 0 = c
nth (_ : cs) n = nth cs (n - 1)

------------ merge ------------

class Merge a where
  merge :: a -> a -> a
  infixr 0 `merge`
instance (Merge Picture) where merge = (CodeWorld.&)
instance (Merge [a]) where merge = (++)

-- merge [] cs' = cs'
-- merge (c : cs) cs' = c : merge cs cs'

applyRange :: forall a. Merge a => (Integer, Integer) -> (Integer -> a) -> a
applyRange (start, end) fn
  | start == end = fn end
  | start < end = go (\x -> x + 1)
  | start > end = go (\x -> x - 1)
 where
  go next = fn start `merge` applyRange (next start, end) fn

------------ coordinates and directions ------------

data Coords = C Integer Integer deriving (Eq)

data Direction = R | U | L | D deriving (Eq)

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

maze1 :: Coords -> Tile
maze1 (C x y)
  | abs x > 4 || abs y > 4 = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x == 2 && y <= 0 = Wall
  | x == 3 && y <= 0 = Storage
  | x >= -2 && y == 0 = Box
  | otherwise = Ground
noBoxMaze :: Maze -> Coords -> Tile
noBoxMaze (Maze _ maze) c = case maze c of
  Box -> Ground
  other -> other

mazeWithBoxes :: Maze -> [Coords] -> Coords -> Tile
mazeWithBoxes maze cs c
  | c `elem` cs = Box
  | otherwise = noBoxMaze maze c

scanMaze :: Merge a => (Coords -> a) -> a
scanMaze fn =
  applyRange (-10, 10) $ \x ->
    applyRange (-10, 10) $ \y -> fn (C x y)

------------ state ------------

data State = S Direction Coords [Coords] Integer deriving (Eq)
loadLevel :: Integer -> State
loadLevel n =
  let Maze c _ = level; level = nth mazes n
   in S R c (initialBoxList level) n

initialBoxList :: Maze -> [Coords]
initialBoxList maze = findTiles Box maze coordsList

storageList :: Maze -> [Coords]
storageList maze = findTiles Storage maze coordsList

coordsList :: [Coords]
coordsList = scanMaze (: [])

findTiles :: Tile -> Maze -> [Coords] -> [Coords]
findTiles tile (Maze _ maze) = filterList (\c -> maze c == tile)

------------ event handling ------------

moveFromTo :: Eq a => a -> a -> a -> a
moveFromTo c0 c1 c = if c0 == c then c1 else c

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) (S _ startC boxList level)
  | key `member` dirMap && not (boxList `subset` storageList maze) =
      S d finalC newBoxList level
  | boxList `subset` storageList maze
      && key == " "
      && level + 1 < listLength mazes =
      loadLevel $ level + 1
 where
  d = dirMap ! key

  finalC = if canMove then targetC else startC
  targetC = adjacentCoords d startC

  canMove = isOk targetTile || targetTile == Box && isOk adjTile
  targetTile = mazeWithBoxes maze boxList targetC
  adjTile = mazeWithBoxes maze boxList (adjacentCoords d targetC)

  isOk tile = case tile of
    Ground -> True
    Storage -> True
    _ -> False

  maze = nth mazes level

  newBoxList = map (moveFromTo targetC (adjacentCoords d finalC)) boxList
handleEvent _ s = s

------------ drawing ------------

fromTile :: Tile -> Picture
data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq)
fromTile tile = case tile of
  Wall -> colored grey (solidRectangle 1 1)
  Ground -> colored yellow (solidRectangle 1 1)
  Storage -> colored black (solidCircle 0.3) & fromTile Ground
  Box -> colored brown (solidRectangle 1 1)
  _ -> blank

(@>) :: Picture -> Coords -> Picture
(@>) pic (C x y) = translated (fromIntegral x) (fromIntegral y) pic

drawMaze :: Maze -> Picture
drawMaze maze = scanMaze $ \c -> fromTile (noBoxMaze maze c) @> c

drawBoxes :: [Coords] -> Picture
drawBoxes cs = fromMaybe blank $ reduce' merge id $ map (fromTile Box @>) cs

player :: Picture
player = colored red (styledLettering Bold Monospace ">")

drawState :: State -> Picture
drawState (S d c boxList level)
  | boxList `subset` storageList maze =
      if level + 1 >= listLength mazes
        then drawPage "You won!" "press esc to restart"
        else drawPage "Level completed!" "press space to continue"
  | otherwise = drawPlayer & drawBoxes boxList & drawMaze maze
 where
  maze = nth mazes level
  drawPlayer = rotated theta player @> c
  theta = case d of
    R -> 0
    U -> pi / 2
    L -> pi
    D -> 3 * pi / 2

drawPage :: Text -> Text -> Picture
drawPage a b =
  scaled 3 3 (lettering a)
    & lettering b
    @> C 0 (-3)

------------ the complete activity ------------

sokoban :: Activity State
sokoban = Activity (loadLevel 0) handleEvent drawState

------------ the general activity type ------------

data Activity world
  = Activity
      world
      (Event -> world -> world)
      (world -> Picture)

runActivity :: Activity s -> IO ()
runActivity (Activity initial handler draw) =
  activityOf initial handler draw

------------ modified activities ------------

resetable :: Activity s -> Activity s
resetable (Activity initial handler draw) =
  Activity initial handler' draw
 where
  handler' (KeyPress "Esc") _ = initial
  handler' e s = handler e s

data WithUndo a = WithUndo a [a]
withUndo :: Eq a => Activity a -> Activity (WithUndo a)
withUndo (Activity state0 handle draw) =
  Activity state0' handle' draw'
 where
  state0' = WithUndo state0 []

  handle' (KeyPress key) (WithUndo s stack) | key == "U" =
    case stack of
      [] -> WithUndo s []
      (s' : stack') -> WithUndo s' stack'
  handle' e (WithUndo s stack)
    | s' == s = WithUndo s stack
    | otherwise = WithUndo (handle e s) (s : stack)
   where
    s' = handle e s

  draw' (WithUndo s _) = draw s

------------ start screen ------------

startScreen :: Picture
startScreen = drawPage "Sokoban!" "press space to start"

data SSState world = StartScreen | Running world deriving (Eq)

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
main = runActivity (resetable $ withUndo $ withStartScreen sokoban)

------------------------------------ levels ------------------------------------

data Maze = Maze Coords (Coords -> Tile)

mazes :: [Maze]
mazes =
  [ Maze (C (-3) 3) maze1
  , Maze (C 1 1) maze9
  , Maze (C (-2) 4) maze6
  , Maze (C (-4) 3) maze3
  , Maze (C 1 (-3)) maze4
  , Maze (C (-3) 3) maze7
  , Maze (C 0 1) maze5
  , Maze (C 0 0) maze8
  ]

maze4 :: Coords -> Tile
maze4 (C x y)
  | abs x > 4 || abs y > 4 = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x == 2 && y < 0 = Wall
  | x >= -1 && y == 1 && x <= 2 = Wall
  | x == -3 && y == 1 = Wall
  | x == 0 && y == 3 = Wall
  | x == 0 && y == 0 = Wall
  | x == 3 && y == -3 = Storage
  | x == 1 && y == 2 = Storage
  | x == -3 && y == 2 = Storage
  | x == 1 && y == -1 = Storage
  | x == -2 && y == 1 = Box
  | x == 2 && y == 2 = Box
  | x <= 1 && y == -2 && x >= 0 = Box
  | otherwise = Ground

maze5 :: Coords -> Tile
maze5 (C x y)
  | abs x > 4 || abs y > 4 = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x == 1 && y < 0 = Wall
  | x == -3 && y == -2 = Wall
  | x <= 1 && x > -2 && y == 0 = Wall
  | x > -3 && x < 3 && y == 2 = Wall
  | x == 3 && y > 1 = Storage
  | y == -2 && x < 0 = Box
  | y == -2 && x == 2 = Box
  | y == 0 && x == 3 = Box
  | y == -1 && x > 1 && x < 4 = Storage
  | otherwise = Ground

maze6 :: Coords -> Tile
maze6 (C x y)
  | abs x > 3 || abs y > 5 = Blank
  | abs x == 3 || abs y == 5 && abs x < 4 = Wall
  | x == 0 && abs y < 4 = Storage
  | x == -1 && (y == 0 || abs y == 2) = Box
  | x == 1 && (abs y == 1 || abs y == 3) = Box
  | x == (-2) && y == 1 = Wall
  | otherwise = Ground

maze7 :: Coords -> Tile
maze7 (C x y)
  | abs x > 4 || abs y > 4 = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x /= 2 && y == 2 = Wall
  | x /= -2 && y == -1 = Wall
  | x == 3 && y == -3 = Storage
  | x == 2 && y == 2 = Box
  | otherwise = Ground

maze8 :: Coords -> Tile
maze8 (C x y)
  | abs x > 10 || abs y > 10 = Blank
  | x == 0 && y == 0 = Ground
  | abs x == 9 && abs y == 9 = Wall
  | abs x == 10 || abs y == 10 = Wall
  | x == y = Storage
  | abs x == abs y = Box
  | x < 0 && x > (-9) && y == 0 = Box
  | x > 0 && x < 9 && y == 0 = Storage
  | otherwise = Ground

maze9 :: Coords -> Tile
maze9 (C x y)
  | abs x > 4 || abs y > 4 = Blank
  | abs x == 4 || abs y == 4 || x == -3 = Wall
  | x == -2 && (y == 3 || y == 0) = Wall
  | x == -1 && y == -1 = Wall
  | x == -0 && y == 1 = Wall
  | x == 3 && y == 0 = Wall
  | x < 0 && (y == 2 || y == -3) = Storage
  | x == -1 && y == 1 = Storage
  | x == 0 && (y == 2 || y == 0 || y == -1) = Box
  | x == 1 && y == -2 = Box
  | x == 2 && y == -3 = Box
  | otherwise = Ground

maze3 :: Coords -> Tile -- bro wtf?
maze3 (C (-5) (-5)) = Wall
maze3 (C (-5) (-4)) = Wall
maze3 (C (-5) (-3)) = Wall
maze3 (C (-5) (-2)) = Wall
maze3 (C (-5) (-1)) = Wall
maze3 (C (-5) 0) = Wall
maze3 (C (-5) 1) = Wall
maze3 (C (-5) 2) = Wall
maze3 (C (-5) 3) = Wall
maze3 (C (-5) 4) = Wall
maze3 (C (-4) (-5)) = Wall
maze3 (C (-4) (-4)) = Ground
maze3 (C (-4) (-3)) = Ground
maze3 (C (-4) (-2)) = Ground
maze3 (C (-4) (-1)) = Ground
maze3 (C (-4) 0) = Ground
maze3 (C (-4) 1) = Ground
maze3 (C (-4) 2) = Ground
maze3 (C (-4) 3) = Ground
maze3 (C (-4) 4) = Wall
maze3 (C (-3) (-5)) = Wall
maze3 (C (-3) (-4)) = Ground
maze3 (C (-3) (-3)) = Wall
maze3 (C (-3) (-2)) = Wall
maze3 (C (-3) (-1)) = Wall
maze3 (C (-3) 0) = Wall
maze3 (C (-3) 1) = Ground
maze3 (C (-3) 2) = Wall
maze3 (C (-3) 3) = Ground
maze3 (C (-3) 4) = Wall
maze3 (C (-3) 5) = Wall
maze3 (C (-2) (-5)) = Wall
maze3 (C (-2) (-4)) = Box
maze3 (C (-2) (-3)) = Ground
maze3 (C (-2) (-2)) = Ground
maze3 (C (-2) (-1)) = Ground
maze3 (C (-2) 0) = Wall
maze3 (C (-2) 1) = Ground
maze3 (C (-2) 2) = Box
maze3 (C (-2) 3) = Box
maze3 (C (-2) 4) = Ground
maze3 (C (-2) 5) = Wall
maze3 (C (-1) (-6)) = Wall
maze3 (C (-1) (-5)) = Wall
maze3 (C (-1) (-4)) = Ground
maze3 (C (-1) (-3)) = Ground
maze3 (C (-1) (-2)) = Ground
maze3 (C (-1) (-1)) = Ground
maze3 (C (-1) 0) = Wall
maze3 (C (-1) 1) = Ground
maze3 (C (-1) 2) = Ground
maze3 (C (-1) 3) = Box
maze3 (C (-1) 4) = Ground
maze3 (C (-1) 5) = Wall
maze3 (C (-1) 6) = Wall
maze3 (C 0 (-6)) = Wall
maze3 (C 0 (-5)) = Ground
maze3 (C 0 (-4)) = Ground
maze3 (C 0 (-3)) = Ground
maze3 (C 0 (-2)) = Ground
maze3 (C 0 (-1)) = Ground
maze3 (C 0 0) = Wall
maze3 (C 0 1) = Wall
maze3 (C 0 2) = Wall
maze3 (C 0 3) = Wall
maze3 (C 0 4) = Ground
maze3 (C 0 5) = Ground
maze3 (C 0 6) = Wall
maze3 (C 1 (-6)) = Wall
maze3 (C 1 (-5)) = Ground
maze3 (C 1 (-4)) = Ground
maze3 (C 1 (-3)) = Ground
maze3 (C 1 (-2)) = Ground
maze3 (C 1 (-1)) = Ground
maze3 (C 1 0) = Wall
maze3 (C 1 1) = Storage
maze3 (C 1 2) = Storage
maze3 (C 1 3) = Storage
maze3 (C 1 4) = Ground
maze3 (C 1 5) = Ground
maze3 (C 1 6) = Wall
maze3 (C 2 (-6)) = Wall
maze3 (C 2 (-5)) = Wall
maze3 (C 2 (-4)) = Ground
maze3 (C 2 (-3)) = Ground
maze3 (C 2 (-2)) = Ground
maze3 (C 2 (-1)) = Ground
maze3 (C 2 0) = Wall
maze3 (C 2 1) = Wall
maze3 (C 2 2) = Wall
maze3 (C 2 3) = Wall
maze3 (C 2 4) = Wall
maze3 (C 2 5) = Wall
maze3 (C 2 6) = Wall
maze3 (C 3 (-5)) = Wall
maze3 (C 3 (-4)) = Ground
maze3 (C 3 (-3)) = Ground
maze3 (C 3 (-2)) = Storage
maze3 (C 3 (-1)) = Ground
maze3 (C 3 0) = Wall
maze3 (C 4 (-5)) = Wall
maze3 (C 4 (-4)) = Wall
maze3 (C 4 (-3)) = Wall
maze3 (C 4 (-2)) = Wall
maze3 (C 4 (-1)) = Wall
maze3 (C 4 0) = Wall
maze3 _ = Blank