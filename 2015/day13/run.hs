{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

import Data.Void (Void)
import Data.List (permutations)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

type Person = String

people :: [Person]
people = [ "Alice"
         , "Bob"
         , "Carol"
         , "David"
         , "Eric"
         , "Frank"
         , "George"
         , "Mallory"
         ]

data Pairing = P Person Person Int
  deriving Show

toTuple :: Pairing -> ((Person, Person), Int)
toTuple (P p1 p2 i) = ((p1, p2), i)

type Settings = HashMap (Person, Person) Int

person :: Parser String
person = choice $ map (try.string) people

sign :: Parser Int
sign = (\x -> if x == "gain" then 1 else -1) <$> choice [string "gain", string "lose"]

pairing :: Parser Pairing
pairing = do
 p1 <- person
 _ <- string " would "
 s <- sign
 _ <- space
 v <- read <$> many digitChar
 _ <- string " happiness units by sitting next to "
 p2 <- person
 pure (P p1 p2 (s*v))


parseAll :: String -> [Pairing]
parseAll = unsafeRight . traverse (parse pairing "") . lines

-- slightly inefficient
happiness :: Settings -> (Person, Person) -> Int
happiness m (p1, p2) = lkp (p1, p2) + lkp (p2, p1)
  where lkp ps = HashMap.lookupDefault 0 ps m

unsafeRight (Right x) = x

process :: [String] -> [Pairing] -> Int
process (anchor:rest) input = maximum
                              . map value
                              $ map (anchor:) (permutations rest)
  where settings = HashMap.fromList . map toTuple $ input
        value xs = sum
                   . map (happiness settings)
                   $ zip xs (drop 1 xs ++ take 1 xs)

part1 :: [Pairing] -> Int
part1 = process people

part2 :: [Pairing] -> Int
part2 = process ("Me":people)

main = do
   input <- parseAll <$> readFile "input.txt"
   print $ part1 input
   print $ part2 input
