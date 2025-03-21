{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Data.Bifunctor
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

type N = Int

parse = map (read @N) . words

parseAll = map parse  . lines

safe r =
  let diffs = zipWith (-) r (drop 1 r)
  in
    all (\v -> v >= 1 && v <= 3) (map abs diffs)
    && length (nub (map signum diffs)) == 1

part1 = length . filter safe

dropped = go
  where go =
          \case []   -> [[]]
                x:xs -> xs:(map (x:) (go xs))

part2 =
  length
  . filter (\xs -> any safe (xs:dropped xs))

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
