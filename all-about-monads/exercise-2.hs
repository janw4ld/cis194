{-# OPTIONS_GHC -Wall -Wimplicit-prelude -Wno-unrecognised-pragmas #-}

import Prelude

import Control.Applicative ((<|>))

---------------------------- exercise 2 ----------------------
{-
Write functions parent and grandparent that should return
one sheep selected from all sheep matching the description or
`Nothing` if there's no such sheep.
-}

-- everything you need to know about sheep
data Sheep = Sheep
  { name :: String
  , mother :: Maybe Sheep
  , father :: Maybe Sheep
  }

-- we show sheep by name
instance Show Sheep where
  show :: Sheep -> String
  show s = show (name s)

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

parent :: Sheep -> Maybe Sheep
parent s = mother s <|> father s

grandparent :: Sheep -> Maybe Sheep
grandparent s = parent s >>= parent {-
                                    foldl1 (<|>) $
                                      map
                                        (>>= parent)
                                        [ mother s -- TODO rewrite this with tuples
                                        , father s
                                        ] -}

main :: IO ()
main = do
  let dolly = breedSheep
  print (parent dolly)
  print (grandparent dolly)
