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

foodP :: Parser (Set String, Set String)
foodP = do
  ingredients <- endBy1 (many letterChar) space1
  allergies <- between (string "(contains ")
                       (char ')')
                       (sepBy1 (many letterChar) (string ", "))
  pure (Set.fromList ingredients, Set.fromList allergies)


parseAll :: String -> [(Set String, Set String)]
parseAll =
  map (\(Right x) -> x)
  . map (parse foodP "")
  . lines


allergeneOptions :: [(Set String, Set String)]
                 -> Map String (Set String)
allergeneOptions =
  Map.unionsWith Set.intersection
  . map (\(is, as) -> Map.fromList . Set.toList $ Set.map (,is) as)

uniqueAssignments :: Map String (Set String) -> Map String String
uniqueAssignments =
  Map.fromList
  . concatMap (\(i, a) -> map (,i) (Set.toList a))
  . Map.toList
  . Map.filter ((== 1) . Set.size)

assign :: [(Set String, Set String)] -> Map String String
assign foods = fst $ fixpoint go (Map.empty, allergeneOptions foods)
  where go (assigned, remaining) =
          let newUnique = uniqueAssignments remaining
              newUniqueAllergenes = Map.keysSet newUnique
              assigned' = assigned <> newUnique
              remaining' =
                Map.filter (not . Set.null)
                . Map.map (`Set.difference` newUniqueAllergenes)
                $ remaining
          in (assigned', remaining')

part1 :: [(Set String, Set String)] -> Int
part1 foods =
  let appearances = counter $ concatMap (Set.toList . fst) foods
      allIngredients = Map.keysSet appearances
      matched = Set.unions . Map.elems $ allergeneOptions foods
      excluded = appearances `Map.withoutKeys` matched
  in sum $ Map.elems excluded

part2 :: [(Set String, Set String)] -> String
part2 = intercalate "," . map fst . sortOn snd . Map.toList . assign

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   putStrLn (part2 input)
