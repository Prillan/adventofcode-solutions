{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (guard)
import Data.List (inits, tails)
import Data.String (IsString)
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

newtype Atom = Atom String
  deriving (Show, Eq, Ord, IsString)

newtype Molecule = Molecule { atoms :: [Atom] }
  deriving (Show, Eq, Ord)

data Rule = Rule !Atom !Molecule deriving Show

azl :: [Char]
azl = ['a'..'z']

azu :: [Char]
azu = ['A'..'Z']

atom :: Parser Atom
atom = Atom <$> ((:) <$> oneOf azu <*> many (oneOf azl))

molecule :: Parser Molecule
molecule = Molecule <$> many atom

eAtom :: Parser Atom
eAtom = Atom <$> string "e"

rule :: Parser Rule
rule = Rule <$> (atom <|> eAtom) <*> (string " => " *> molecule)

unsafeRight (Right x) = x

parseAll :: String -> ([Rule], Molecule)
parseAll input = ( unsafeRight . traverse (parse rule "") $ r
                 , unsafeRight (parse molecule "" l)
                 )
  where l = last . lines $ input
        r = init . filter (not . null) . lines $ input

foci :: [a] -> [([a], a, [a])]
foci [] = []
foci xs =
  map (\case (ys, z:zs) -> (ys, z, zs))
  . init
  $ zip (inits xs) (tails xs)

step :: [Rule] -> Molecule -> Set.Set Molecule
step rules input = Set.fromList do
  Rule a (Molecule replacement) <- rules
  (h, e, t) <- foci (atoms input)
  guard $ a == e
  pure $ Molecule $ h ++ replacement ++ t

part1 :: [Rule] -> Molecule -> Int
part1 rules = length . step rules

part2 :: Molecule -> Int
part2 (Molecule input) = symbols - lefts - rights - (2*ys) - 1
  where symbols = length input
        lefts = length $ filter (== "Rn") input
        rights = length $ filter (== "Ar") input
        ys = length $ filter (== "Y") input

main = do
   (rules, input) <- parseAll <$> readFile "input.txt"
   print (part1 rules input)
   print (part2 input)
