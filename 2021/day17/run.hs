{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Data.Foldable
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe

type TargetArea = (V2 Int, V2 Int)

-- TODO: Optimize, the bounds in the solution could be better.
-- TODO: Replace each V2 in the target area with Ix?

parseRange str =
  case splitOn ".." (drop 2 str ) of
    [l, h] -> v2 (read @Int l, read @Int h)

parse input = 
  case stripPrefix "target area: " (head . lines $ input) of
    Just xy -> case splitOn ", " xy of
                 [x, y] -> (parseRange x, parseRange y)

inRange :: V2 Int -> Int -> Ordering
inRange (V2 (rl, rh)) p
  | p < rl    = LT
  | p > rh    = GT
  | otherwise = EQ

status :: TargetArea -> V2 Int -> (Ordering, Ordering)
status (txr, tyr) (V2 (px, py)) = (inRange txr px, inRange tyr py)

step :: (V2 Int, V2 Int) -> (V2 Int, V2 Int)
step (vel@(V2 (vx, vy)), pos) =
  (V2 (max 0 (vx - 1), vy - 1), pos + vel)

simulate :: V2 Int -> V2 Int -> TargetArea -> [(V2 Int, V2 Int)]
simulate vel pos target =
  takeWhile (not . noReturn) $ iterate step (vel, pos)
  where noReturn (v, p) =
          case (v, status target p) of
            (_, (GT, _))         -> True
            (_, (_, LT))         -> True
            (V2 (0, _), (LT, _)) -> True
            _                    -> False

reachesTarget :: TargetArea -> [(V2 Int, V2 Int)] -> Maybe (V2 Int, V2 Int)
reachesTarget target = find ((== (EQ, EQ)) . status target . snd)

part1 target@(trx, try) =
  let maxx = maximum trx
      minx = 0
      maxy = 500
      miny = 0

      candidates = (,) <$> [minx..maxx] <*> [miny..maxy]
  in
    maximum
    . map (\(_, V2 (_, y)) -> y)
    . concat
    . filter (isJust . reachesTarget target)
    . map (\c -> simulate (v2 c) 0 target)
    $ candidates
  
part2  target@(trx, try) =
  let maxx = maximum trx
      minx = 0
      maxy = 500
      miny = minimum try

      candidates = (,) <$> [minx..maxx] <*> [miny..maxy]
  in
    length
    . filter (isJust . reachesTarget target)
    . map (\c -> simulate (v2 c) 0 target)
    $ candidates

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
   input <- parse <$> readFile file
   print (part1 input)
   print (part2 input)
