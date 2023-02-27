{-# OPTIONS_GHC -Wall -Wimplicit-prelude #-}

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
(//) :: a -> Stream a -> Stream a
(//) = Cons

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
streamInterleave (Cons x xs) ys =
  Cons
    x
    ( Cons
        (streamHead ys)
        (streamInterleave xs (streamTail ys))
    )

--  where
streamHead :: Stream a -> a
streamHead (Cons x' _) = x'
streamTail :: Stream a -> Stream a
streamTail (Cons _ xs') = xs'

------------------------------ Exercise 3 ------------------------------
