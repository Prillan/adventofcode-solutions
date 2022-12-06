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

-- TODO: Clean up.

diff :: Int -> String -> Int
diff n = go 0
  where
    go acc xs =
      case splitAt n xs of
        (marker, []) -> acc + length marker
        (marker, rest)
          | length (nub marker) == n -> acc + n
          | otherwise -> go (acc + 1) (drop 1 marker ++ rest)

part1 = diff 4
part2 = diff 14

main :: IO ()
main = main' "input.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- readFile file
   print (part1 input)
   print (part2 input)
