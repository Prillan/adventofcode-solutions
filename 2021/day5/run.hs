{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Control.Applicative (liftA2)
import Data.Bits (xor)
import Data.Ord (comparing)
import Data.Bifunctor
import Data.Function ((&))
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

type Line = (V2 Int, V2 Int)

parse :: String -> Line
parse input =
  let [[fx, fy], [tx, ty]] = map (map read . splitOn ",") $ splitOn " -> " input
  in (v2 (fx, fy), v2 (tx, ty))

parseAll = map parse  . lines

hv (f, t) = any (== 0) (f - t)

diag (f, t) = let V2 (x, y) = abs <$> f - t
              in x == y

range :: Int -> Int -> [Int]
range f t | f <= t    = [f..t]
          | otherwise = [f,f-1..t]

lspan :: Line -> [V2 Int]
lspan v@(V2 (fx, fy), V2 (tx, ty))
  | hv v   = sequence $ v2 (range fx tx, range fy ty)
  | diag v = map v2 $ zip (range fx tx) (range fy ty)

part1 = length
        . Map.filter (> 1)
        . counter
        . concat
        . map lspan
        . filter hv

part2 = length
        . Map.filter (> 1)
        . counter
        . concat
        . map lspan
        . filter ((||) <$> hv <*> diag)

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
