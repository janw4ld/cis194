{-# OPTIONS_GHC -Wall -Wimplicit-prelude -Wno-unrecognised-pragmas #-}

import Prelude

{- A function to reverse a list -}
rev :: Foldable f => f a -> [a]
rev = foldl (flip (:)) []

{- A function to return all prefixes of a list -}
prefixes :: Show a => [a] -> [[a]]
prefixes = foldr (\x acc -> [x] : map (x :) acc) []

{- Interpolation polynomial in lagrange form -}
lagrange :: [(Float, Float)] -> Float -> Float
lagrange xs x =
  foldl
    ( \acc (xj, y) ->
        acc
          + y * l xj
    )
    0
    xs
 where
  l xj =
    foldl
      ( \acc (xm, _) ->
          acc
            * ( if xm /= xj
                  then (x - xm) / (xj - xm)
                  else 1
              )
      )
      1 -- multiplication
      xs

{- A function that folds the elements of a trie in a preorder traversal -}
data Trie a = Leaf a | Node a [Trie a]
foldtrie :: (b -> a -> b) -> b -> Trie a -> b
foldtrie = undefined