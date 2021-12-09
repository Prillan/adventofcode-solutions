{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
import Control.Applicative

import Data.List (permutations, minimumBy, maximumBy, maximum)
import Data.Maybe (fromJust)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Text.Parsec hiding ((<|>))

type Person = String
persons = ["Alice", "Bob", "Carol", "David"
          , "Eric", "Frank", "George", "Mallory"]
data Pairing = P Person Person Integer
  deriving Show
toTuple (P p1 p2 i) = ((p1, p2), i)

data Arrangement = A [Person] Integer
  deriving (Show, Eq)
instance Ord Arrangement where
  compare (A _ x) (A _ y) = compare x y

type Settings = HashMap (Person, Person) Integer

person :: Stream s m Char => ParsecT s u m String
person = choice $ map (try.string) persons

sign :: Stream s m Char => ParsecT s u m Integer
sign = (\x -> if x == "gain" then 1 else -1) <$> choice [string "gain", string "lose"]

pairing :: Stream s m Char => ParsecT s u m Pairing
pairing = do
 p1 <- person
 _ <- string " would "
 s <- sign
 _ <- space
 v <- read <$> many1 digit
 _ <- string " happiness units by sitting next to "
 p2 <- person
 pure (P p1 p2 (s*v))

readPairing :: String -> Either ParseError Pairing
readPairing = parse pairing ""

unsafeLookup m (p1, p2) = fromJust $ HashMap.lookup (p1, p2) m <|> HashMap.lookup (p2, p1) m
happiness m (p1, p2) = fromJust $ (+) <$> lkp (p1, p2) <*> lkp (p2, p1)
  where lkp (p1, p2) = HashMap.lookup (p1, p2) m

unsafeRight (Right x) = x

process input = optimal
  where setting = HashMap.fromList . map (toTuple . unsafeRight . readPairing) . lines $ input
        arrangements = map (head (persons):) (permutations (drop 1 persons))
        value xs = sum . map (happiness setting) $ zip xs (drop 1 xs ++ [head xs])
        toArrangement xs = A xs (value xs)
        optimal = maximum . map toArrangement $ arrangements

main = do
   input <- readFile "input.txt"
   print (process input)
