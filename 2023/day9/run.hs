{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

-- TODO :Cleanup

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

nums = map (read @N) . words

parse = nums

parseAll = map parse  . lines

diffs xs = zipWith (-) (drop 1 xs) xs

solve input = go input [last input]
  where go xs hist
          | all (== 0) xs = hist
          | otherwise =
              let ds = diffs xs
              in go ds (last ds:hist)

solve' input = go input [head input]
  where go xs hist
          | all (== 0) xs = hist
          | otherwise =
              let ds = diffs xs
              in go ds (head ds:hist)

part1 = sum . map (sum . solve)
part2 = sum . map (foldl f 0 . solve')
  where f acc v = v - acc

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
