{-# OPTIONS_GHC -Wall -Wno-implicit-prelude -Wno-unrecognised-pragmas #-}

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