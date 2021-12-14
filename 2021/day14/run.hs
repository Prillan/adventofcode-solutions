{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

type Rules = HashMap (Char, Char) Char

parseRule :: String -> ((Char, Char), Char)
parseRule line =
  case splitOn " -> " line of
    [[c1, c2], to] -> ((c1, c2), head to)

parseAll :: String -> (String, Rules)
parseAll input =
  case break null (lines input) of
    ([start], rules) -> (start, HashMap.fromList . map parseRule $ drop 1 rules)

pairCounts :: String -> Counter (Char, Char)
pairCounts input = counter $ zip input (drop 1 input)

step :: Rules -> Counter (Char, Char) -> Counter (Char, Char)
step rules =
  Map.fromListWith (+)
  . concatMap (\(p, n) -> let new = rules HashMap.! p
                          in [((fst p, new), n), ((new, snd p), n)])
  . Map.toList

charCounts :: Char -> Char -> Counter (Char, Char) -> Counter Char
charCounts h l =
  Map.map (`div` 2)
  . Map.mapWithKey (\case c | c == h || c == l -> (+ 1)
                            | otherwise        -> id)
  . Map.fromListWith (+)
  . concatMap (\((c1, c2), n) -> [(c1, n), (c2, n)])
  . Map.toList

countsAfter :: Rules -> Int -> String -> Counter Char
countsAfter rules n input =
    charCounts (head input) (last input)
    . iterateN n (step rules)
    $ pairCounts input

solve :: Int -> (String, Rules) -> Int
solve n (input, rules) =
  let counts = countsAfter rules n input
      most   = maximum counts
      least  = minimum counts
  in most - least

part1 :: (String, Rules) -> Int
part1 = solve 10

part2 :: (String, Rules) -> Int
part2 = solve 40

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
