{-# OPTIONS_GHC -Wall -Wno-implicit-prelude -Wno-unrecognised-pragmas #-}

import Prelude

------------------------------ Exercise 1 ------------------------------

fib' :: Integer -> Integer
fib' 0 = 1
fib' 1 = 1
fib' n = fib (n - 1) + fib (n - 2)
fibs' :: [Integer]
fibs' = map fib [0 ..]

---
fibs :: [Integer]
fibs =
  1
    : 1
    : zipWith (+) fibs (tail fibs)
fib :: Integer -> Integer
fib = (!!) fibs . fromInteger

------------------------------ Exercise 2 ------------------------------

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show (Cons x xs) = show x ++ ", " ++ show xs

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

toStream :: Foldable f => f a -> Stream a
toStream = foldr Cons (error "UNREACHABLE!! Streams can't end")

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap fn (Cons x xs) = Cons (fn x) (streamMap fn xs)

streamIterate :: (a -> a) -> a -> Stream a
streamIterate fn x = Cons x (streamIterate fn (fn x))

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons x xs) ys = Cons x (streamInterleave ys xs)

--  where
streamHead :: Stream a -> a
streamHead (Cons x' _) = x'
streamTail :: Stream a -> Stream a
streamTail (Cons _ xs') = xs'

nats :: Stream Integer
nats = streamIterate (+ 1) 0

-- TODO ruler function
ruler :: Stream Integer
{-
|0    |    0  |       0 |           0 |               0 |  <-- (streamRepeat 0)
|+1->1|0  1   |0 1 0 2  |0 1 0 2 0 1  |0 1 0 2 0 1 0 3  |0 1 0 2 0 1 0 3 0 1 <- ruler
|     |  +1->2|   +1-->1|     +1---->3|       +1------>1|  <-- (+1) `streamMap` ruler
-}
ruler = streamInterleave (streamRepeat 0) (streamMap (+ 1) ruler)

------------------------------ Exercise 3 ------------------------------

newtype Supply s a = S (Stream s -> (a, Stream s))

get :: Supply s s
get = S (\(Cons x xs') -> (x, xs'))

pureSupply :: a -> Supply s a
pureSupply x = S (x,) -- Tuple-section, equivalent to \xs -> (x,xs)

mapSupply :: (a -> b) -> Supply s a -> Supply s b
mapSupply fn (S s) =
  S
    ( \xs ->
        let (x, xs') = s xs
         in (fn x, xs')
    )

mapSupply2 :: (a -> b -> c) -> Supply s a -> Supply s b -> Supply s c
mapSupply2 fn (S sA) (S sB) =
  S
    ( \xs ->
        let
          (a, as) = sA xs
          (b, bs) = sB as
         in
          (fn a b, bs)
    )

bindSupply :: Supply s a -> (a -> Supply s b) -> Supply s b
bindSupply (S sA) fn =
  S
    ( \xs ->
        let
          (x, xs') = sA xs
          (S sB) = fn x
         in
          sB xs'
    )

runSupply :: Stream s -> Supply s a -> a
runSupply xs (S s) = fst $ s xs

instance Functor (Supply s) where
  fmap = mapSupply

instance Applicative (Supply s) where
  pure = pureSupply
  (<*>) = mapSupply2 id

instance Monad (Supply s) where
  return = pure
  (>>=) = bindSupply

data Tree a = Node (Tree a) (Tree a) | Leaf a deriving (Show)
labelTree :: Tree a -> Tree Integer
labelTree t = runSupply nats (go t)
 where
  go :: Tree a -> Supply s (Tree s)
  go (Node lt rt) = do
    lt' <- go lt
    rt' <- go rt
    return (Node lt' rt')
  go (Leaf _) = S $ \(Cons x s') -> (Leaf x, s')