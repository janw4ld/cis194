---------------------------- exercise 1 ----------------------
{- Replace the use of do notation with monadic operators -}

-- everything you need to know about sheep
data Sheep = Sheep
  { name :: String
  , mother :: Maybe Sheep
  , father :: Maybe Sheep
  }

-- we show sheep by name
instance Show Sheep where
  show s = show (name s)

maternalGrandfather :: Sheep -> Maybe Sheep
maternalGrandfather s = mother s >>= father

fathersMaternalGrandmother :: Sheep -> Maybe Sheep
fathersMaternalGrandmother s = father s >>= mother >>= mother

mothersPaternalGrandfather :: Sheep -> Maybe Sheep
mothersPaternalGrandfather s = mother s >>= father >>= father

-- this builds our sheep family tree
breedSheep :: Sheep
breedSheep =
  let adam = Sheep "Adam" Nothing Nothing
      eve = Sheep "Eve" Nothing Nothing
      uranus = Sheep "Uranus" Nothing Nothing
      gaea = Sheep "Gaea" Nothing Nothing
      kronos = Sheep "Kronos" (Just gaea) (Just uranus)
      holly = Sheep "Holly" (Just eve) (Just adam)
      roger = Sheep "Roger" (Just eve) (Just kronos)
      molly = Sheep "Molly" (Just holly) (Just roger)
   in Sheep "Dolly" (Just molly) Nothing

-- print Dolly's maternal grandfather
main :: IO ()
main =
  let dolly = breedSheep
   in print (maternalGrandfather dolly)