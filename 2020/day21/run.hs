{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Data.Bits (xor)
import Data.Ord (comparing)
import Data.Bifunctor
import Data.Maybe
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

type Parser = Parsec Void String

foodP :: Parser ([String], [String])
foodP = do
  ingredients <- endBy1 (many letterChar) space1
  allergies <- between (string "(contains ")
                       (char ')')
                       (sepBy1 (many letterChar) (string ", "))
  pure (ingredients, allergies)

parseAll =
  map (\(Right x) -> x)
  . map (parse foodP "")
  . lines

allergeneOptions =
  Map.unionsWith Set.intersection
  . map (\(is, as) -> Map.fromList $ [ (a, Set.fromList is) | a <- as])

assign foods = fst $ fixpoint go (Map.empty, allergeneOptions foods)
  where go (assigned, remaining) =
          let newUnique =
                Map.fromList
                . concatMap (\(i, a) -> (,i) <$> Set.toList a)
                . Map.toList
                . Map.filter ((== 1) . Set.size)
                $ remaining
              newUniqueAllergenes = Map.keysSet newUnique
              assigned' = Map.union assigned newUnique
              remaining' =
                Map.filter (not . Set.null)
                . Map.map (`Set.difference` newUniqueAllergenes)
                $ remaining
          in (assigned', remaining')

part1 foods =
  let appearances = counter $ concatMap fst foods
      allIngredients = Map.keysSet appearances
      matched = Set.unions . Map.elems $ allergeneOptions foods
      excluded = appearances `Map.withoutKeys` matched
  in sum $ Map.elems excluded
part2 = intercalate "," . map fst . sortOn snd . Map.toList . assign

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   putStrLn (part2 input)
