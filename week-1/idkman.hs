{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}

import CodeWorld
import GHC.Data.StringBuffer (StringBuffer (cur))

main :: IO ()
main = exercise3

-- Fill in the blanks! (When I say blanks, I mean undefineds)

-- Exercise 1

botCircle, topCircle :: Color -> Picture
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
-- data Tile = Wall | Ground | Storage | Box | Blank
drawTile :: Integer -> Picture
drawTile n
  | n == 1 = wall
  | n == 2 = ground
  | n == 3 = storage
  | n == 4 = box
  | otherwise = blank

hmmm :: Integer -> Integer -> Picture
hmmm x y = translated (fromIntegral x) (fromIntegral y) (drawTile (maze x y))

drawRow :: Integer -> Integer -> Picture
drawRow (-10) y = hmmm (-10) y
drawRow x y = hmmm x y & drawRow (x - 1) y

drawCol :: Integer -> Integer -> Picture
drawCol x (-10) = drawRow x (-10)
drawCol x y = drawRow x y & drawCol x (y - 1)

-- x in [-10, 10]
exercise3 :: IO ()
exercise3 = drawingOf (drawCol 10 10)

maze :: Integer -> Integer -> Integer
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2