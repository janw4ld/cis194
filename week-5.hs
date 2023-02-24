{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use null" #-}
{-# OPTIONS_GHC -Wall -Wimplicit-prelude -Wno-unrecognised-pragmas #-}

import Data.Char qualified as C
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Set qualified as S
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import Prelude

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
  , sumNumbers "words0are1234separated12by3integers45678" == 46927
  , sumNumbers "000a." == 0
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
{-
Use the following example output as specification:

Number of lines: 23
Number of empty lines: 10
Number of words: 40
Number of unique words: 25
Number of words followed by themselves: 3
Length of the longest line: 5

A line and a word is what `lines` respectively `words` return.
-}
wordCount str =
  "Word Stats: "
    ++ "\nNo. of lines: "
    ++ show (length ls)
    ++ "\nNo. of empty lines: "
    ++ show (length (filter null ls))
    ++ "\nNo. of words: "
    ++ show (length ws)
    ++ "\nNo. of unique words: "
    ++ show (length $ S.fromList ws)
    -- ++ "\nNo. of words followed by themselves"
    -- ++ show $ T.foldl
    ++ "\nLongest line length: "
    ++ show (maximum $ map length ls)
    ++ "\n"
 where
  ls = lines str
  ws = words str

---------------------------------- Exercise 3 ----------------------------------

{-
testResults :: [(String, [Bool])]
testResults =
  [ ("halveEvens", ex_halveEvens)
  , ("safeString", ex_safeString)
  , ("holes", ex_holes)
      …
  ]

as well as a function `formatTests` that presents the data nicely.
```
halveEvens: 3/3 successful tests
safeString: 1/3 successful tests. Failing tests: 1, 3 and 4
holes: All 2 tests failed.
```
Define main to print the string returned by formatTests applied to testResults.

(Naturally, all your tests are failing. You can add some bogus data to
testResults to test your formatTests function.)
-}

formatTests :: [(String, [Bool])] -> String
formatTests = undefined
