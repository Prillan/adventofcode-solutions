{-# LANGUAGE BangPatterns, TypeApplications #-}
import Data.Bits (xor)
import Data.Ord (comparing)
import Data.Bifunctor (bimap)
import Data.Maybe
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


data Seat = Floor | Empty | Occupied
  deriving (Show, Eq)

parse '.' = Floor
parse 'L' = Empty
parse '#' = Occupied

count e = length . filter (e ==)

type Grid = Map (Int, Int) Seat

parseAll = toGrid . map (map parse) . lines

toGrid :: [[Seat]] -> Grid
toGrid xs =
  Map.fromList [((ci, ri), cell) | (ri, row) <- zip [0..] xs
                                 , (ci, cell) <- zip [0..] row]

dirs =
  [ (-1, -1)
  , (-1, 0)
  , (-1, 1)
  , (0, -1)
  , (0, 1)
  , (1, -1)
  , (1, 0)
  , (1, 1) ]

neighbors :: Grid -> (Int, Int) -> [Seat]
neighbors g (c, r) =
  mapMaybe (flip Map.lookup g)
  . map (\(dx, dy) -> (c + dx, r + dy))
  $ dirs

rule1 g pos s =
  case (s, neighbors g pos) of
    (Empty, xs)
      | count Occupied xs == 0 -> Occupied
    (Occupied, xs)
      | count Occupied xs >= 4 -> Empty
    _ -> s

nearest :: Grid -> (Int, Int) -> (Int, Int) -> Maybe Seat
nearest g (ci, ri) (dx, dy) =
  listToMaybe
  . mapMaybe id
  . dropWhile (== Just Floor)
  . takeWhile isJust
  . map (flip Map.lookup g)
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

step rule g = let !r = Map.mapWithKey (rule g) g in r

fixpoint f a =
  let xs = iterate f a
  in fst . head . filter (uncurry (==)) $ zip (drop 1 xs) xs

pp Empty = 'L'
pp Occupied = '#'
pp Floor = '.'

printGrid g =
  let (cs, rs) = unzip $ Map.keys g
      cm = maximum cs
      rm = maximum rs
  in
    unlines
    . map (map pp)
    . map (map (g Map.!))
    $ [[(ci, ri) | ci <- [0..cm]]  | ri <- [0..rm]]

part1 =
  count Occupied
  . Map.elems
  . fixpoint (step rule1)
part2 =
  count Occupied
  . Map.elems
  . fixpoint (step rule2)

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
