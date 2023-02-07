{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}

import           CodeWorld
import           GHC.Data.StringBuffer (StringBuffer (cur))

main :: IO ()
main = exercise3

-- Fill in the blanks! (When I say blanks, I mean undefineds)

-- Exercise 1

botCircle, midCircle, topCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-3) (solidCircle 1))
midCircle c = colored c (translated 0 0 (solidCircle 1))
topCircle c = colored c (translated 0 3 (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 9

trafficLight :: Int -> Picture
trafficLight 5 = botCircle green & topCircle black & midCircle black & frame
trafficLight 3 = botCircle black & topCircle red & midCircle yellow & frame
trafficLight 2 = botCircle black & topCircle red & midCircle black & frame
trafficLight 0 = botCircle black & topCircle black & midCircle yellow & frame

trafficController :: Double -> Picture
trafficController t
  | round t `mod` 6 == 5 || round t `mod` 6 == 4 = trafficLight 5
  | round t `mod` 6 == 3 = trafficLight 3
  | round t `mod` 6 == 2 || round t `mod` 6 == 1 = trafficLight 2
  | otherwise = trafficLight 0

trafficLightAnimation :: Double -> Picture
trafficLightAnimation = trafficController

exercise1 :: IO ()
exercise1 = animationOf trafficLightAnimation

-- Exercise 2

tree :: Integer -> Picture -> Picture
tree 0 blossom = blossom
tree n blossom =
  curve [(0, 0), (0, 1)]
    & translated
      0
      1
      ( rotated (pi / 10) (tree (n - 1) blossom)
          & rotated (-pi / 10) (tree (n - 1) blossom)
      )

blossom :: Double -> Picture
blossom t = colored green (solidCircle (min t 10 / 5))

exercise2 :: IO ()
exercise2 = animationOf (tree 8 . blossom)

-- Exercise 3

box, wall, ground, storage :: Picture
box = colored brown (solidRectangle 1 1)
wall = colored grey (solidRectangle 1 1)
ground = colored yellow (solidRectangle 1 1)
storage = colored black (solidCircle 0.3) & ground
data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq)

getTile :: Tile -> Picture
getTile n
  | n == Wall = wall
  | n == Ground = ground
  | n == Storage = storage
  | n == Box = box
  | otherwise = blank

data Coords = C Integer Integer
drawTile :: Integer -> Integer -> Picture
drawTile x y = translated (fromIntegral x) (fromIntegral y) (getTile (maze (C x y)))

drawRow :: Coords -> Picture
drawRow (C (-10) y) = drawTile (-10) y
drawRow (C x y)     = drawTile x y & drawRow (C (x - 1) y)

drawGrid :: Coords -> Picture
drawGrid (C x (-10)) = drawRow (C x (-10))
drawGrid (C x y)     = drawRow (C x y) & drawGrid (C x (y - 1))

exercise3 :: IO ()
exercise3 = drawingOf (drawGrid (C 10 10))

maze :: Coords -> Tile
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x == 2     && y <= 0     = Wall
  | x == 3     && y <= 0     = Storage
  | x >= -2    && y == 0     = Box
  | otherwise                = Ground
