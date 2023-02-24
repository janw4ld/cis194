{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use null" #-}
{-# OPTIONS_GHC -Wall -Wimplicit-prelude -Wno-unrecognised-pragmas #-}

import Data.Char qualified as C
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.List (findIndices)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import Prelude

-- import System.Environment (getArgs)

---------------------------------- Exercise 1 ----------------------------------

ex_halveEvens :: [Bool]
ex_halveEvens =
  [ halveEvens [] == []
  , halveEvens [1, 2, 3, 4, 5] == [1, 2]
  , halveEvens [6, 6, 6, 3, 3, 3, 2, 2, 2] == [3, 3, 3, 1, 1, 1]
  ]

halveEvens :: [Integer] -> [Integer]
halveEvens [] = []
halveEvens xs = map (`div` 2) (filter even xs)

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

holes :: forall a. [a] -> [[a]]
holes xs =
  map
    ( \n ->
        if 0 <= n && n < m
          then take n xs ++ drop (n + 1) xs
          else []
    )
    [0 .. m - 1] ---TODO: find scanner-ish functions
 where
  m = length xs

-- holeAt idx xs = lft ++ drop 1 rgt
--  where
--   (lft, (_ : rgt)) = splitAt idx xs ---- ! non-exhaustive

ex_longestText :: [Bool]
ex_longestText =
  [ longestText [True, False] == False
  , longestText [2, 4, 16, 32] == (32 :: Int)
  , longestText (words "Hello World") == "World"
  , longestText (words "Olá mundo") == "Olá"
  ]

longestText :: Show a => [a] -> a
longestText [] = error "Input can't be empty"
longestText xs = maximumBy (compare `on` length . show) xs --- *** return to this sorcery

ex_adjacents :: [Bool]
ex_adjacents =
  [ adjacents "" == []
  , adjacents [True] == []
  , adjacents "Hello" == [('H', 'e'), ('e', 'l'), ('l', 'l'), ('l', 'o')]
  ]

adjacents :: forall a. [a] -> [(a, a)]
adjacents xs =
  [ (i, j) -- I'm doing really cursed stuff at this point, but the real zip is weird
  | i <- xs
  | j <- drop 1 xs
  ]

ex_commas :: [Bool]
ex_commas =
  [ commas [] == ""
  , commas ["Hello"] == "Hello"
  , commas ["Hello", "World"] == "Hello, World"
  , commas ["Hello", "", "World"] == "Hello, , World"
  , commas ["Hello", "new", "World"] == "Hello, new, World"
  ]

commas :: [String] -> String
commas [] = ""
commas xs = concatMap (++ ", ") (init xs) ++ last xs

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
sumNumbers str =
  sum $
    map (toInteger . strToInt) $
      filter (/= "") $ -- not the most elegant or performant solution
        T.split (not . C.isDigit) (T.pack str)
 where
  strToInt :: Text -> Int
  strToInt =
    T.foldl
      ( \a c ->
          a * 10 + C.digitToInt c
      )
      0

---------------------------------- Exercise 2 ----------------------------------
wordCount :: String -> String
{- ORMOLU_DISABLE -}
wordCount str =
  "Word Stats: "
    ++ "\nNumber of lines: "        ++ show (length ls)
    ++ "\nNumber of empty lines: "  ++ show (length (filter null ls))
    ++ "\nNumber of words: "        ++ show (length ws)
    ++ "\nNumber of unique words: " ++ show (length $ S.fromList ws)
    ++ "\nNumber of words followed by themselves: " ++ show (dupeCount ws S.empty) -- this is wrong
    ++ "\nLength of the longest line: " ++ show (maximum $ map length ls)
    ++ "\n"
 where
  ls = lines str
  ws = words str
  dupeCount :: [String] -> Set String -> Int
-- dupeCount xs = length $ filter (not.(`elem` (S.toList.S.fromList) xs)) xs
  dupeCount (w:w':ws') xs
    | w==w' = dupeCount (w':ws') (w`S.insert`xs)
    | otherwise = dupeCount (w':ws') xs
  dupeCount _ xs = length xs
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
    count = show . length
    successCount rs = count $ filter id rs
    failedList = commas . map (show.(+1)) $ failedIndices
    failedIndices = findIndices not results
{- ORMOLU_ENABLE -}