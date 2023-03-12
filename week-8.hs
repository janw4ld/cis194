{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Wimplicit-prelude -Wno-unrecognised-pragmas #-}

import Control.Monad (unless, void)
import Data.Char (isAlphaNum)
import Data.Maybe (catMaybes)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
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

--------------------------------------------------------------------------------
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
{- NO SOLUTION -}
func9 xs ys zs = xs >>= \x -> if even x then ys else zs

-- func9' :: Applicative f => f Integer -> f Integer -> f Integer -> f Integer
-- func9' xs ys zs =
--   ( \(y, z) x ->
--       if even x then y else z
--   )
--     <$> ( (,)
--             <$> ys
--             <*> zs -- this is incorrect
--         )
--     <*> xs

func10 :: Monad f => f Integer -> f Integer
func10 xs = do
  x <- xs >>= (\x -> return (x * x))
  return (x + 10)

func10' :: Functor f => f Integer -> f Integer
func10' xs = (\x -> x * x + 10) <$> xs

--------------------------------------------------------------------------------

{-
Parser a is function that takes the remaining input string and returns either
Nothing when parsing fails, or the next value of type a
-}
newtype Parser a = P (String -> Maybe (a, String)) -- kind * -> *

runParser :: Parser a -> String -> Maybe (a, String)
{- parser indirection, avoids pattern matching it to keep it lazy -}
runParser (P p) = p

parse :: Parser a -> String -> Maybe a
{-
the main entry point to the parser, it shall reutrn only if the parser consumed
the whole input succesfully
-}
parse p str = case runParser p str of
  Just (result, "" {- parsing complete -}) -> Just result
  _ -> Nothing

noParser :: Parser a
{-
an always failing parser where
>>>parse noParser _ == Nothing
-}
noParser = P $ const Nothing

pureParser :: a -> Parser a
{-
A parser that consumes no input and returns its argument
>>>parse (pureParser x) "" == Just x
(xs /= "") => parse (pureParser x) xs == Nothing
-}
pureParser token = P $ \str -> Just (token, str)

{- ORMOLU_DISABLE -}
instance Functor Parser where
  {- >>>parse (f <$> p) input == f <$> (parse p input) -}
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap fn p = P $ \str -> do
    (token, str') <- runParser p str
    pure (fn token, str') -- both are the exact same
    -- P (runParser p >=> (\(token, str') -> pure (fn token, str')))

instance Applicative Parser where
  pure :: a -> Parser a
  pure = pureParser
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  fp <*> fx = P $ \str -> do
      (f, str') <- runParser fp str
      (token, str'') <- runParser fx str'
      return (f token, str'')

instance Monad Parser where
  -- return = pure
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  fa >>= k = P $ \str -> do
      (token, str') <- runParser fa str
      runParser (k token) str'
{- ORMOLU_ENABLE -}

{-
>>>parse anyChar "" == Nothing
>>>parse anyChar [c] == Just c
length xs > 1 =>
  parse anyChar xs == Nothing -- this is handled by `parse` not anyChar!
-}
anyChar :: Parser Char
anyChar = P $ \case
  (token : str') -> Just (token, str')
  [] -> Nothing

char :: Char -> Parser ()
char c = do
  token <- anyChar
  unless (token == c) noParser

anyCharBut :: Char -> Parser Char
anyCharBut c = do
  token <- anyChar
  if token /= c then pure token else noParser

orElse :: Parser a -> Parser a -> Parser a
{- tries the first parser, then tries the second only if the first fails -}
orElse p1 p2 = P $ \str ->
  let result = runParser p1 str
   in case result of
        Nothing -> runParser p2 str
        _ -> result

many :: Parser a -> Parser [a]
{-
applies a given parser as often as possible until it fails and returns all
results
-}
many p = ((:) <$> p <*> many p) `orElse` return []

--  where
-- collect tokens = (:) <$> p >>= (\token -> collect (tokens <> [token]))
-- collect tokens = p >>= (\token -> collect (tokens <> {- redundant -} [token])) . (:)

sepBy :: Parser a -> Parser () -> Parser [a]
sepBy p1 p2 = ((:) <$> p1 <*> many (p2 >> p1)) `orElse` return []

parseCSV :: Parser [[String]]
parseCSV = many $ (p `sepBy` char ',') <* char '\n'
 where
  p = do
{- ORMOLU_DISABLE -}
    char '"'; token <- many (anyCharBut '"'); char '"'
    return token
{- ORMOLU_ENABLE -}

--------------------------------------------------------------------------------
type Identifer = String
type Declaration = (Identifer, String)
type Section = (Identifer, [Declaration])
type INIFile = [Section]

parseINI :: Parser INIFile
parseINI = many $ do
  trimWhite
  char '['
  identifier <- many pAlphaNum
  char ']'
  trimWhite
{- ORMOLU_DISABLE -}
  declarations <-
    many $ do
        key <- many pAlphaNum
        trimSpaces ; char '=' ; trimSpaces
        value <- many (anyCharBut '\n') ; trimWhite
        return $ Just (key, value)
      `orElse` ((char '#' >> anyCharBut '\n' >> trimWhite) >> return Nothing)
  return (identifier, catMaybes declarations)
{- ORMOLU_ENABLE -}
 where
  trimSpaces = (void . many) (char ' ')
  trimWhite = (void . many) (char '\n' `orElse` char ' ')
  pAlphaNum = anyChar >>= \c -> if isAlphaNum c then return c else noParser

main :: IO ()
main = do
  args <- getArgs
  input <- case args of
    [] -> getContents
    [fileName] -> readFile fileName
    _ -> hPutStrLn stderr "Too many arguments given" >> exitFailure
  case parse parseINI input of
    Just i -> print i
    Nothing -> do
      hPutStrLn stderr "Failed to parse INI file."
      exitFailure
