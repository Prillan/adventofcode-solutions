{-# LANGUAGE BangPatterns, TypeApplications #-}
import AoC
import AoC.Grid

import Data.Bits (xor)
import Data.Ord (comparing)
import Data.Bifunctor (bimap)
import Data.Maybe
import Data.List
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

data Seat = Floor | Empty | Occupied
  deriving (Show, Eq)

parse '.' = Floor
parse 'L' = Empty
parse '#' = Occupied

count e = length . filter (e ==)

parseAll = parseMapGrid parse

dirs =
  [ (-1, -1)
  , (-1, 0)
  , (-1, 1)
  , (0, -1)
  , (0, 1)
  , (1, -1)
  , (1, 0)
  , (1, 1) ]

neighbors g (c, r) =
  mapMaybe (flip HashMap.lookup g)
  . map (\(dx, dy) -> (c + dx, r + dy))
  $ dirs

rule1 g pos s =
  case (s, neighbors g pos) of
    (Empty, xs)
      | count Occupied xs == 0 -> Occupied
    (Occupied, xs)
      | count Occupied xs >= 4 -> Empty
    _ -> s

nearest g (ci, ri) (dx, dy) =
  listToMaybe
  . mapMaybe id
  . dropWhile (== Just Floor)
  . takeWhile isJust
  . map (flip HashMap.lookup g)
  . drop 1
  . iterate (bimap (+ dx) (+ dy))
  $ (ci, ri)

rule2 g pos s =
  case (s, mapMaybe (nearest g pos) dirs) of
    (Empty, xs)
      | count Occupied xs == 0 -> Occupied
    (Occupied, xs)
      | count Occupied xs >= 5 -> Empty
    _ -> s

step rule g = let !r = HashMap.mapWithKey (rule g) g in r

part1 =
  count Occupied
  . HashMap.elems
  . fixpoint (step rule1)
part2 =
  count Occupied
  . HashMap.elems
  . fixpoint (step rule2)

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
