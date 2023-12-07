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

type N = Integer
type F = Double

t2 [x, y] = (x, y)

parse = map (read @F) . drop 1 . words

parseAll = t2 . map parse . lines

parseJoined = read @F . concat . drop 1 . words
parseOne = t2 . map parseJoined . lines

-- x*(t-x) > d
-- -x² + xt - d > 0
-- x² - xt + d < 0   (
-- x = ½t +- sqrt(t²/4  - d)
-- ½t - sqrt(t²/4 - d) < x < ½t + sqrt(t²/4 - d)

ineq t d =
  let root = sqrt (t^2 / 4 - d)
  in
    (t/2 - root, t/2 + root)

rounded (l, u) = (ceiling l, floor u)
counts (l, u) = u - l + 1

solve =
  counts . rounded . uncurry ineq

part1 (times, distances) =
  product . map solve $ zip times distances

part2 = solve

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- readFile file
   print (part1 $ parseAll input)
   print (part2 $ parseOne input)
