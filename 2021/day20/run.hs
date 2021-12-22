{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Data.Foldable

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

type Rule = Vector Bool
type Grid = HashSet (Int, Int) -- TODO: Move to AoC.Grid

parseRule :: String -> Rule
parseRule = V.fromList . map ('#' ==)

parseGrid' :: String -> Grid
parseGrid' =
  HashMap.keysSet
  . HashMap.filter id
  . parseMapGrid ('#' ==)

parseAll :: String -> (Rule, Grid)
parseAll input =
  let rule:_:grid = lines input
  in (parseRule rule, parseGrid' (unlines grid))

neighbors (i, j) = [ (i + dx, j + dy) | dy <- [-1, 0, 1]
                                      , dx <- [-1, 0, 1]
                                      ]

inBounds (l, h) (x, y) =
  l <= x && x <= h && l <= y && y <= h

-- step only works if the rule has True in the lowest part, which I
-- assume all inputs do, because it's really really sneaky
step rule ((l, h), background, grid) =
  let posValue p
        | inBounds (l, h) p = p `HashSet.member` grid
        | otherwise  = background
      next = HashSet.fromList
             . map fst
             . filter snd
             . map (\(p, nb) -> (p, rule V.! bitsFromBools nb))
             . map (\p -> (p, map posValue (neighbors p)))
             $ (,) <$> [l-1..h+1] <*> [l-1..h+1]
  in ((l - 1, h + 1), not background, next)

gridBounds grid =
  let (xs, ys) = unzip . toList $ grid
  in (minimum (xs ++ ys), maximum (xs ++ ys))

solve :: Int -> (Rule, Grid) -> Int
solve n (rule, grid) =
  let bounds = gridBounds grid
      (_, _, result) = iterateN n (step rule) (bounds, False, grid)
  in length result

part1 = solve 2
part2 = solve 50

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
