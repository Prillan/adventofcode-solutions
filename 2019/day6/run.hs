module Main (main) where

import Control.Arrow ((***), (&&&), second)
import Data.List
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

type Orbit = (String, String)

parse :: String -> Orbit
parse = (id *** drop 1) . break (== ')')

parseAll :: String -> [Orbit]
parseAll = map parse . lines

children :: [Orbit] -> HashMap String [String]
children = HashMap.fromListWith (++) . map (second pure)

parent :: [Orbit] -> HashMap String String
parent = HashMap.fromList . map (snd &&& fst)

nodes :: HashMap String [String] -> HashSet String
nodes orbitedBys =
  HashSet.union (HashMap.keysSet orbitedBys)
                (HashSet.fromList $ concat $ HashMap.elems orbitedBys)

loeb :: Functor a => a (a x -> x) -> a x
loeb x = fmap (\a -> a (loeb x)) x

buildCountSolver :: HashMap String [String] -> HashMap String (HashMap String Int -> Int)
buildCountSolver orbitedBys =
  let (others, leafs) =
        partition (`HashMap.member` orbitedBys)
        . HashSet.toList
        $ nodes orbitedBys
      leafCells = zip leafs (repeat (const 0))
      otherCells = map (\n -> (n, step n)) others
      step n counts =
        sum . map (+ 1) . map (counts HashMap.!) $ orbitedBys HashMap.! n
  in
    HashMap.fromList $ leafCells ++ otherCells

path :: HashMap String String -> String -> [String]
path _ [] = []
path _ "COM" = ["COM"]
path parents leaf =
  leaf:path parents (parents HashMap.! leaf)

part1 :: [Orbit] -> Int
part1 = sum . HashMap.elems . loeb . buildCountSolver . children

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
