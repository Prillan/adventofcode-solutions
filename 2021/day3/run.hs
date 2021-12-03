{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
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

parseAll :: String -> [String]
parseAll = lines

gamma :: [String] -> Int
gamma = readBinary . map mostCommon . transpose

epsilon :: [String] -> Int
epsilon = readBinary . map leastCommon . transpose

part1 :: [String] -> Int
part1 input = gamma input * epsilon input

locate :: ([Char] -> Char) -> [String] -> String
locate selector = go 0
  where go _ [x] = x
        go i xs = let m = selector . map (!! i) $ xs
                      rem = filter (\x -> x !! i == m) xs
                  in go (i + 1) rem

oxyGen :: [String] -> Int
oxyGen = readBinary . locate mostCommon

co2Scrub :: [String] -> Int
co2Scrub = readBinary . locate leastCommon

part2 :: [String] -> Int
part2 input = oxyGen input * co2Scrub input

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
