module Main (main) where

import Control.Arrow ((***), (&&&), second)
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Orbit = (String, String)

parse :: String -> Orbit
parse = (id *** drop 1) . break (== ')')

parseAll :: String -> [Orbit]
parseAll = map parse . lines

children :: [Orbit] -> Map String [String]
children = Map.fromListWith (++) . map (second pure)

parent :: [Orbit] -> Map String String
parent = Map.fromList . map (snd &&& fst)

nodes :: Map String [String] -> Set String
nodes orbitedBys =
  Set.union (Map.keysSet orbitedBys)
            (Set.fromList $ concat $ Map.elems orbitedBys)

loeb :: Functor a => a (a x -> x) -> a x
loeb x = fmap (\a -> a (loeb x)) x

buildCountSolver :: Map String [String] -> Map String (Map String Int -> Int)
buildCountSolver orbitedBys =
  let (others, leafs) =
        partition (`Map.member` orbitedBys) . Set.toList $ nodes orbitedBys
      leafCells = zip leafs (repeat (const 0))
      otherCells = map (\n -> (n, step n)) others
      step n counts =
        sum . map (+ 1) . map (counts Map.!) $ orbitedBys Map.! n
  in
    Map.fromList $ leafCells ++ otherCells

path :: Map String String -> String -> [String]
path _ [] = []
path _ "COM" = ["COM"]
path parents leaf =
  leaf:path parents (parents Map.! leaf)

part1 :: [Orbit] -> Int
part1 = sum . Map.elems . loeb . buildCountSolver . children

part2 :: [Orbit] -> Int
part2 orbits =
  let parents = parent orbits
      youPath = reverse $ path parents "YOU"
      sanPath = reverse $ path parents "SAN"

      commonPrefix =
        map fst
        . takeWhile (uncurry (==))
        $ zip youPath sanPath

      Just you = stripPrefix (init commonPrefix) youPath
      Just san = stripPrefix (init commonPrefix) sanPath
  in
    case (you, san) of
      ([_, "YOU"], [_, "SAN"]) -> 0
      ([_, "YOU"], _:rest) -> length rest - 1
      (_:rest, [_, "SAN"]) -> length rest - 1
      _ -> (length you - 2) + (length san - 1) - 1

main :: IO ()
main = do
  input <- parseAll <$> readFile "input.txt"
  print (part1 input)
  print (part2 input)
