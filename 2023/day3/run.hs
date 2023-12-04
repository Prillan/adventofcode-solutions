{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- TODO: Clean up

import AoC
import AoC.Grid

import Data.Char (isDigit)
import Data.Bifunctor
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Maybe
import Control.Monad (guard)
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

parse = HashMap.filter (/= '.') . parseMapGrid id

neighbors (ci, ri) =
  [ (ci + cd, ri + rd) | rd <- [-1, 0, 1]
                       , cd <- [-1, 0, 1]
                       , not (rd == 0 && cd == 0)
                       ]

collect numbers (n@(ci, ri), x) =
  let l = leftsOf numbers n
      r = rightsOf numbers n
      res = l <> [x] <> r
  in
    ( (ci - length l, ri)
    , length res
    , read @N res
    )

leftsOf numbers = go
  where go (ci, ri) =
          let next = (ci - 1, ri)
          in case numbers HashMap.!? next of
            Just c | isDigit c -> go next <> [c]
            _ -> []

rightsOf numbers = go
  where go (ci, ri) =
          let next = (ci + 1, ri)
          in case numbers HashMap.!? next of
            Just c | isDigit c -> c:go next
            _ -> []

partNumbers g =
  let numbers = HashMap.filter isDigit g
      symbols = HashMap.keys $ g `HashMap.difference` numbers
      startNums = do
        s <- symbols
        n <- neighbors s
        x <- toList $ numbers HashMap.!? n
        pure (n, x)
  in
    Set.fromList $ map (collect numbers) startNums


gearNumbers g =
  let parts = toList $ partNumbers g
      -- (ci, ri) -> part
      partMap = HashMap.fromList do
        p@((ci, ri), len, _) <- parts
        i <- [0..len - 1]
        pure ((ci + i, ri), p)
      gearSyms = HashMap.keys $ HashMap.filter (== '*') g
  in do
    gear <- gearSyms
    let adj = toList $ Set.fromList do
          n <- neighbors gear
          p <- toList $ partMap HashMap.!? n
          pure p
    guard $ length adj == 2
    pure (gear, map thd adj)

thd (_, _, z) = z

part1 = sum . map thd . toList . partNumbers
part2 = sum . map (product . snd) . gearNumbers

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parse <$> readFile file
   print (part1 input)
   print (part2 input)
