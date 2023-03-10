{-# OPTIONS_GHC -Wall -Wimplicit-prelude -Wno-unrecognised-pragmas -Wunrecognised-pragmas #-}

import Data.List (sort)

import Test.QuickCheck -- <https://hackage.haskell.org/package/QuickCheck>
import Prelude

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

invTupTree :: Tree (Integer, Integer)
invTupTree = buildTree (0, 0)
 where
  buildTree (a, b) =
    Node
      (buildTree (a + 1, b))
      (a, b)
      (buildTree (a, b + 1))

cut :: Integer -> Tree a -> Tree a
cut 0 _ = Leaf
cut _ Leaf = Leaf
cut n (Node l x r) = Node (cut (n - 1) l) x (cut (n - 1) r)

------------------------------------------------------------------------
{- binary tree construction -}
insert :: Ord a => a -> Tree a -> Tree a
insert el Leaf = Node Leaf el Leaf
insert el (Node l x r)
  | el > x = Node l x (insert el r)
  | el <= x = Node (insert el l) x r
insert _ tree = tree -- ??

-- insert el tree = trace (show el ++ show tree) tree -- trace unexpected patterns
-- \| otherwise = Node l x r -- is it normal behaviour to dedupe? -- turns out no

inOrder :: Tree a -> [a]
inOrder Leaf = []
inOrder (Node l x r) = inOrder l ++ [x] ++ inOrder r

prop_sorted :: Ord a => [a] -> Bool
prop_sorted xs = sort xs == inOrder (foldr insert Leaf xs)

check :: IO ()
check =
  quickCheckWith
    stdArgs{maxSuccess = 10000}
    (prop_sorted :: [Int] -> Bool)