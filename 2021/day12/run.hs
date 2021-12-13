{-# LANGUAGE LambdaCase #-}
import AoC
import AoC.Grid

import Data.Char
import Data.Foldable
import Data.List

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

type Node = String

parse :: String -> HashMap Node [Node]
parse =
  HashMap.map (sort . nub)
  . HashMap.fromListWith (++)
  . concatMap f
  . lines
  where f n = case break (== '-') n of
                (a, '-':b) -> [ (a, [b]), (b, [a]) ]

paths :: (HashMap Node Int -> Node -> Bool)
      -> HashMap Node [Node]
      -> [[Node]]
paths canVisit edges = go ["start"] HashMap.empty "start"
  where go visited counts = \case
          "end"   -> [visited]
          current ->
            concatMap (\case n@(h:_) | isLower h -> go (n:visited) (HashMap.insertWith (+) n 1 counts) n
                                     | otherwise -> go (n:visited) counts n)
            . filter (canVisit counts)
            . concat
            . toList
            $ HashMap.lookup current edges

part1 = length . paths canVisit
  where canVisit visited = \case
          n@(h:_) | n == "start" -> False
                  | n == "end"   -> True
                  | isLower h    -> not (n `HashMap.member` visited)
                  | otherwise    -> True

part2 = length . paths canVisit
  where canVisit visited = \case
          n@(h:_) | n == "start" -> False
                  | n == "end"   -> True
                  | isLower h    -> not (n `HashMap.member` visited) || all (< 2) (toList visited)
                  | otherwise    -> True

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
   input <- parse <$> readFile file
   print (part1 input)
   print (part2 input)
