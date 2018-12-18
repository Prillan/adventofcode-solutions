{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative
import Control.Monad (replicateM)
import Data.Maybe (mapMaybe)

data Tree a = Tree { children :: [Tree a],
                     metadata :: [a] }
  deriving (Show, Functor, Foldable, Traversable)

newtype Parser a = Parser { runParser :: [Int] -> [(a, [Int])] }
  deriving Functor

instance Applicative Parser where
  pure x = Parser $ \input -> [(x, input)]
  Parser fp <*> Parser xp = Parser $ \input -> do
    (f, fs) <- fp input
    (x, xs) <- xp fs
    pure (f x, xs)

instance Monad Parser where
  return = pure
  Parser x >>= mf = Parser $ \input -> do
    (a, rest) <- x input
    runParser (mf a) rest

instance Alternative Parser where
  empty = Parser $ const []
  (Parser x) <|> (Parser y) = Parser $ \input ->
    case x input of
      [] -> y input
      result -> result

readMetadata :: Int -> Parser [Int]
readMetadata n = replicateM n readInt

readInt :: Parser Int
readInt = Parser $ \input ->
  case input of
    [] -> empty
    (x:xs) -> [(x, xs)]

readTree :: Parser (Tree Int)
readTree = do
  cn <- readInt
  mn <- readInt
  c <- replicateM cn readTree
  m <- readMetadata mn
  return $ Tree { children = c, metadata = m }

parse :: Show a => Parser a -> [Int] -> a
parse p input =
  case runParser p input of
    [] ->
      error $ "Parsing failed!"
    [(x, [])] ->
      x
    _:_:_ ->
      error $ "Ambiguous"
    [(x, rest)] ->
      error $ "Parsed: " ++ show x ++ "\nInput remaining: " ++ show rest

parseAll :: String -> Tree Int
parseAll = parse readTree . map read . words

(!?) :: [a] -> Int -> Maybe a
list !? i =
  case drop i list of
    x:_ -> Just x
    _   -> Nothing

value :: Tree Int -> Int
value Tree {..}
  | null children = sum metadata
  | otherwise     = sum
                    . map value
                    . mapMaybe (children !?)
                    . map (+ (-1)) $ metadata

part1 :: Num a => Tree a -> a
part1 = sum

part2 :: Tree Int -> Int
part2 = value

testInput :: String
testInput = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

main :: IO ()
main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
