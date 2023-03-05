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

mapB :: Functor g => (b->c) -> ComplicatedB f g a b -> ComplicatedB f g a c
mapB _ (Con3 fa) = Con3 fa
mapB fn (Con4 gb) = Con4 $ fn `fmap` gb
mapB fn (Con5 g2bs) = Con5 $ fmap (fmap (map fn)) g2bs