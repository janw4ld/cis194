{-# OPTIONS_GHC -Wall -Wimplicit-prelude -Wno-unrecognised-pragmas #-}

import Prelude

{- implement a functor for both of the following data typess -}

data ComplicatedA a b
  = Con1 a b
  | Con2 [Maybe (a -> b)]

data ComplicatedB f g a b
  = Con3 (f a)
  | Con4 (g b)
  | Con5 (g (g [b]))

mapA :: (b -> c) -> ComplicatedA a b -> ComplicatedA a c
mapA fn (Con1 a b) = Con1 a $ fn b
mapA fn (Con2 [Just fn1]) = Con2 [Just (fn . fn1)]
mapA _ (Con2 _) = Con2 [Nothing]

mapB :: Functor g => (b -> c) -> ComplicatedB f g a b -> ComplicatedB f g a c
mapB _ (Con3 fa) = Con3 fa
mapB fn (Con4 gb) = Con4 $ fn `fmap` gb
mapB fn (Con5 g2bs) = Con5 $ fmap (fmap (map fn)) g2bs

------------------------------------------------------------------------
{- Replace monads in the following with applicatives or functors. -}

func0 :: Monad f => (a -> a) -> f a -> f a
func0 f xs = do
  x <- xs
  return (f (f x))

func0' :: Functor f => (a -> a) -> f a -> f a
func0' f xs = f . f <$> xs

func1 :: Monad f => f a -> f (a, a)
func1 xs = xs >>= (\x -> return (x, x))

func1' :: Functor f => f a -> f (a, a)
func1' xs = (\x -> (x, x)) <$> xs

func2 :: Monad f => f a -> f (a, a)
func2 xs = xs >>= (\x -> xs >>= \y -> return (x, y))

func2' :: Applicative f => f a -> f (a, a)
func2' xs = ((,) <$> xs) <*> xs

func3 :: Monad f => f a -> f (a, a)
func3 xs = xs >>= (\x -> xs >>= \y -> return (x, x))

func3' :: Applicative f => f a -> f (a, a)
func3' xs = (\x _ -> (x, x)) <$> xs <*> xs

func4 :: Monad f => f a -> f a -> f (a, a)
func4 xs ys = xs >>= (\x -> ys >>= \y -> return (x, y))

func4' :: Applicative f => f a -> f a -> f (a, a)
func4' xs ys = (,) <$> xs <*> ys

func5 :: Monad f => f Integer -> f Integer -> f Integer
func5 xs ys = do
  x <- xs
  let x' = x + 1
  y <- (+ 1) <$> ys
  return (x' + y)

func5' :: Applicative f => f Integer -> f Integer -> f Integer
func5' xs ys = (\x y -> x + 1 + y + 1) <$> xs <*> ys

func6 :: Monad f => f Integer -> f (Integer, Integer)
func6 xs = do
  x <- xs
  return $
    if x > 0
      then (x, 0)
      else (0, x)

func6' :: Functor f => f Integer -> f (Integer, Integer)
func6' xs =
  ( \x -> if x > 0 then (x, 0) else (0, x)
  )
    <$> xs

func7 :: Monad f => f Integer -> f (Integer, Integer)
func7 xs = do
  x <- xs
  if x > 0
    then return (x, 0)
    else return (0, x)

func7' :: Functor f => f Integer -> f (Integer, Integer)
func7' xs =
  ( \x -> if x > 0 then (x, 0) else (0, x)
  )
    <$> xs

func8 :: Monad f => f Integer -> Integer -> f Integer
func8 xs x = pure (+) <*> xs <*> pure x

func8' :: Functor f => f Integer -> Integer -> f Integer
func8' xs x = (+ x) <$> xs

func9 :: Monad f => f Integer -> f Integer -> f Integer -> f Integer
func9 xs ys zs = xs >>= \x -> if even x then ys else zs

func9' :: Applicative f => f Integer -> f Integer -> f Integer -> f Integer
func9' xs ys zs =
  ( \(y, z) x ->
      if even x then y else z
  )
    <$> ( (,)
            <$> ys
            <*> zs -- this is incorrect
        )
    <*> xs

func10 :: Monad f => f Integer -> f Integer
func10 xs = do
  x <- xs >>= (\x -> return (x * x))
  return (x + 10)

func10' :: Functor f => f Integer -> f Integer
func10' xs = (\x -> x * x + 10) <$> xs

------------------------------------------------------------------------
