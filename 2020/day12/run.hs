{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Data.Bits (xor)
import Data.Ord (comparing)
import Data.Bifunctor
import Data.Maybe
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

data Action = N Int
            | E Int
            | W Int
            | S Int
            | L Int
            | R Int
            | F Int
  deriving (Show, Read, Eq)

parse (c:rest) = read @Action $ c:' ':rest

parseAll =
  map parse . lines

data State = State { direction :: Int
                   , position :: V2 Int }
  deriving (Show)

initial = State 0 0

move :: State -> Action -> State
move (s@State {..}) = \case
  N x -> s { position = position + v2 (0, x) }
  E x -> s { position = position + v2 (x, 0) }
  W x -> s { position = position + v2 (-x, 0) }
  S x -> s { position = position + v2 (0, -x) }
  L x -> s { direction = (360 + direction + x) `mod` 360}
  R x -> s { direction = (360 + direction - x) `mod` 360}
  F x -> s { position = position + fromIntegral x * fdir }
  where fdir = case direction of
          0   -> v2 (1, 0)
          90  -> v2 (0, 1)
          180 -> v2 (-1, 0)
          270 -> v2 (0, -1)

dist (V2 (x0, y0)) (V2 (x1, y1)) = abs (x0 - x1) + abs (y0 - y1)

part1 = dist 0 . position . foldl move initial

data State2 = State2 { position2 :: V2 Int
                     , waypoint2 :: V2 Int }
  deriving (Show)

initial2 = State2 0 (v2 (10, 1))

move2 :: State2 -> Action -> State2
move2 (s@State2 {..}) = \case
  L x -> s { waypoint2 = rotate x waypoint2 }
  R x -> s { waypoint2 = rotate (negate x) waypoint2 }
  F x -> s { position2 = position2 + fromIntegral x * waypoint2 }
  N x -> s { waypoint2 = waypoint2 + v2 (0, x) }
  E x -> s { waypoint2 = waypoint2 + v2 (x, 0) }
  W x -> s { waypoint2 = waypoint2 + v2 (-x, 0) }
  S x -> s { waypoint2 = waypoint2 + v2 (0, -x) }

rotate deg v@(V2 (x, y))
  | deg == 0 = v
  | deg < 0  = rotate (deg + 360) v
  | deg `mod` 90 == 0 = rotate (deg - 90) (v2 (-y, x))
  | otherwise = error "can only rotate multiples of 90 degs"

part2 = dist 0 . position2 . foldl move2 initial2

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
