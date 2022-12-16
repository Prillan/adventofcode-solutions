{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Control.Applicative (liftA2)

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

-- FIXME: This code is horrible, and gives a false positive. Requires
-- massive cleanup.

parse = id
input = [
  ((3772068, 2853720), (4068389, 2345925))
  ,((78607, 2544104), (-152196, 4183739))
  ,((3239531, 3939220), (3568548, 4206192))
  ,((339124, 989831), (570292, 1048239))
  ,((3957534, 2132743), (3897332, 2000000))
  ,((1882965, 3426126), (2580484, 3654136))
  ,((1159443, 3861139), (2580484, 3654136))
  ,((2433461, 287013), (2088099, 190228))
  ,((3004122, 3483833), (2580484, 3654136))
  ,((3571821, 799602), (3897332, 2000000))
  ,((2376562, 1539540), (2700909, 2519581))
  ,((785113, 1273008), (570292, 1048239))
  ,((1990787, 38164), (2088099, 190228))
  ,((3993778, 3482849), (4247709, 3561264))
  ,((3821391, 3986080), (3568548, 4206192))
  ,((2703294, 3999015), (2580484, 3654136))
  ,((1448314, 2210094), (2700909, 2519581))
  ,((3351224, 2364892), (4068389, 2345925))
  ,((196419, 3491556), (-152196, 4183739))
  ,((175004, 138614), (570292, 1048239))
  ,((1618460, 806488), (570292, 1048239))
  ,((3974730, 1940193), (3897332, 2000000))
  ,((2995314, 2961775), (2700909, 2519581))
  ,((105378, 1513086), (570292, 1048239))
  ,((3576958, 3665667), (3568548, 4206192))
  ,((2712265, 2155055), (2700909, 2519581))
  ]

example =
  [((2, 18), (-2, 15))
  ,((9, 16), (10, 16))
  ,((13, 2), (15, 3))
  ,((12, 14), (10, 16))
  ,((10, 20), (10, 16))
  ,((14, 17), (10, 16))
  ,((8, 7), (2, 10))
  ,((2, 0), (2, 10))
  ,((0, 11), (2, 10))
  ,((20, 14), (25, 17))
  ,((17, 20), (21, 22))
  ,((16, 7), (15, 3))
  ,((14, 3), (15, 3))
  ,((20, 1), (15, 3))
  ]

parseAll = map parse  . lines

toBall (s, b) =
  (v2 s, sum $ abs $ v2 s - v2 b)

--bounds :: [(V2 N, N)] -> (N, N)
bounds balls =
  let xmin = minimum $ map (\(V2 (cx, _), r) -> cx - r) balls
      xmax = maximum $ map (\(V2 (cx, _), r) -> cx + r) balls
  in (xmin, xmax)

inside p (center, r) =
  sum (abs (p - center)) <= r

part1 row i =
  let beacons = map (v2 . snd) i
      balls = map toBall i
      (xmin, xmax) = bounds balls
  in
    length
    . filter (not . (`elem` beacons))
    . filter (\p -> any (p `inside`) balls)
    . map (\x -> v2 (x, row))
    $ [xmin..xmax]


type Interval = (N, N)

slice y (center@(V2 (cx, cy)), r) =
  let ydist = abs $ cy - y
      xmin = cx - r + ydist
      xmax = cx + r - ydist
  in
    if ydist > r
    then Nothing
    else Just (xmin, xmax)

merge :: [Interval] -> [(N, Int)]
merge =
  sortBy (comparing fst <> comparing (negate . snd))
  . concatMap (\(l, h) -> [(l, 1), (h, -1)])

thd (_, _, z) = z

onlyMissing intervals =
  let grouped =
        map (\(xs@((k, _):_)) -> (k, sum $ map snd xs))
        . groupBy (\a b -> fst a == fst b)
        . merge
        $ intervals

  in
    map (\(p1, (p2, v)) -> p1 - 1)
    . filter (\(p1, (p2, v)) -> v == 0 && p1 == p2 + 2)
    . init
    . tail
    . zip (map fst grouped) -- TODO: Remove and merge with below
    . scanl (\(p1, v) (p2, v') -> (p2, v + v')) (0, 0)
    $ grouped

onRow y balls =
  onlyMissing $ mapMaybe (slice y) balls

part2 factor i =
  let beacons = map (v2 . snd) i
      balls = map toBall i
  in
    map (\(y, x) -> (x, y, x * factor + y))
    . concatMap (\y -> sequence (y, onRow y balls))
    $ [0..factor]


eballs = map toBall example
s11 = mapMaybe (slice 11) eballs

grpd = groupBy (\a b -> fst a == fst b) $ merge s11

asdf = init . tail . scanl (\(p1, v) (p2, v') -> (p2, v + v')) (0, 0) $ map f grpd
  where f xs@((k, _):_) = (k, sum $ map snd xs)


main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
--   input <- parseAll <$> readFile file
   print (part1 2000000 input)
   mapM_ print (part2 4000000 input)

   print (part1 10 example)
   print (part2 20 example)
