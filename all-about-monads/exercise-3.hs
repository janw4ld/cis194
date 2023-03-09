{-# OPTIONS_GHC -Wall -Wno-implicit-prelude -Wno-unrecognised-pragmas #-}

-- import Control.Applicative ((<|>))
import Data.Maybe (maybeToList)

---------------------------- exercise 1 ----------------------
{-
Write functions parent and grandparent with signature
Sheep -> [Sheep]. They should return all sheep matching the
description, or the empty list if there is no such sheep.
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

parent :: Sheep -> [Sheep]
parent s =
  -- foldl1 (<|>) $
      [ mother s
      , father s
      ] >>= maybeToList

grandparent :: Sheep -> [Sheep]
grandparent s = parent s >>= parent

main :: IO ()
main = do
  let dolly = breedSheep
  print (parent dolly)
  print (grandparent dolly)
