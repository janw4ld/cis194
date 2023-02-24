{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use null" #-}
{-# OPTIONS_GHC -Wall -Wimplicit-prelude -Wno-unrecognised-pragmas #-}

import Prelude

import Data.Char qualified as C
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.List qualified as L

-- import Data.Set qualified as S
-- import System.Environment (getArgs)

---------------------------------- Exercise 1 ----------------------------------

ex_halveEvens :: [Bool]
ex_halveEvens =
  [ halveEvens [] == []
  , halveEvens [1, 2, 3, 4, 5] == [1, 2]
  , halveEvens [6, 6, 6, 3, 3, 3, 2, 2, 2] == [3, 3, 3, 1, 1, 1]
  ]

halveEvens :: [Integer] -> [Integer]
halveEvens = map (`div` 2) . filter even

ex_safeString :: [Bool]
ex_safeString =
  [ safeString [] == []
  , safeString "Hello World!" == "Hello World!"
  , safeString "That’s your line:\n" == "That_s your line:_"
  , safeString "🙋.o(“Me Me Me”)" == "_.o(_Me Me Me_)"
  ]

safeString :: String -> String
safeString = map $ \c ->
  if C.isAscii c && C.isPrint c
    then c
    else '_'

ex_holes :: [Bool]
ex_holes =
  [ holes "" == []
  , holes "Hello" == ["ello", "Hllo", "Helo", "Helo", "Hell"]
  ]

holes :: [a] -> [[a]]
holes xs =
  [ take i xs ++ drop (i + 1) xs
  | i <- [0 .. length xs - 1]
  ]

ex_longestText :: [Bool]
ex_longestText =
  [ longestText [True, False] == False
  , longestText [2, 4, 16, 32] == (32 :: Int)
  , longestText (words "Hello World") == "World"
  , longestText (words "Olá mundo") == "Olá"
  ]

longestText :: Show a => [a] -> a
longestText = maximumBy (compare `on` (length . show))

ex_adjacents :: [Bool]
ex_adjacents =
  [ adjacents "" == []
  , adjacents [True] == []
  , adjacents "Hello" == [('H', 'e'), ('e', 'l'), ('l', 'l'), ('l', 'o')]
  ]

adjacents :: [a] -> [(a, a)]
adjacents xs = zip xs (tail xs)

ex_commas :: [Bool]
ex_commas =
  [ commas [] == ""
  , commas ["Hello"] == "Hello"
  , commas ["Hello", "World"] == "Hello, World"
  , commas ["Hello", "", "World"] == "Hello, , World"
  , commas ["Hello", "new", "World"] == "Hello, new, World"
  ]

commas :: [String] -> String
commas = L.intercalate ", "

ex_sumNumbers :: [Bool]
ex_sumNumbers =
  [ sumNumbers "" == 0
  , sumNumbers "Hello world!" == 0
  , sumNumbers "a1bc222d3f44" == 270
  , sumNumbers "words0are1234sep3arated12byintegers45678" == 46927
  , sumNumbers "00a." == 0
  , sumNumbers "0.00a." == 0
  ]

sumNumbers :: String -> Integer
sumNumbers =
  sum
    . map read
    . filter (C.isDigit . head)
    . L.groupBy ((==) `on` C.isDigit)

---------------------------------- Exercise 2 ----------------------------------
-- count :: Foldable a -> String
count :: [a] -> String
count = show . length

wordCount :: String -> String
{- ORMOLU_DISABLE -}
wordCount str =
  "Word Stats: "
    ++ "\nNumber of lines: "        ++ count ls
    ++ "\nNumber of empty lines: "  ++ count (filter null ls)
    ++ "\nNumber of words: "        ++ count ws
    ++ "\nNumber of unique words: " ++ count (L.nub ws)
    ++ "\nNumber of words followed by themselves: " ++ count dupes
    ++ "\nLength of the longest line: " ++ show (maximum $ map length ls)
    ++ "\n"
 where
  ls = lines str
  ws = words str
  dupes = filter (uncurry (==)) (adjacents ws)
  -- dupes (w:w':ws') xs -- recursive but more efficient?
  --   | w==w' = dupes (w':ws') (w`S.insert`xs)
  --   | otherwise = dupes (w':ws') xs
  -- dupes _ xs = xs
{- ORMOLU_ENABLE -}

{-
main :: IO ()
main = do
  args <- getArgs
  case args of
    [filepath] -> do
      contents <- readFile filepath
      putStr $ wordCount contents
    _ -> putStrLn "Usage: wordcount filepath"
 -}
---------------------------------- Exercise 3 ----------------------------------

testResults :: [(String, [Bool])]
testResults =
  [ ("halveEvens", ex_halveEvens)
  , ("safeString", ex_safeString)
  , ("holes", ex_holes)
  , ("longestText", ex_longestText)
  , ("adjacents", ex_adjacents)
  , ("commas", ex_commas)
  , ("sumNumbers", ex_sumNumbers)
  ]

formatTests :: [(String, [Bool])] -> String
{- ORMOLU_DISABLE -}
formatTests = concatMap format where
  format :: (String, [Bool]) -> String
  format (name, results) =
    name ++ ": "
    ++ successCount results ++ "/" ++ count results ++ " successful tests. "
    ++ ( if null failedList then "" else
        ( if length failedIndices == 1 
          then "Test " else "Tests "
        ) ++ failedList ++ " failed."
        ) 
    ++ "\n"
   where
    successCount rs = count $ filter id rs
    failedList = commas . map (show.(+1)) $ failedIndices
    failedIndices = L.findIndices not results
{- ORMOLU_ENABLE -}