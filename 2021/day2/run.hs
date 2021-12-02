{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Data.Bifunctor
import Data.Bits (xor)
import Data.Function ((&))
import Data.List
import Data.Maybe
import Data.Ord (comparing)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

data Cmd = F Int | U Int | D Int
  deriving Show

parse :: String -> Cmd
parse line = break (== ' ') line & \case
  ("forward", v)  -> F (read v)
  ("up", v)       -> U (read v)
  ("down", v)     -> D (read v)

parseAll = map parse  . lines

toV2 = \case
  F v -> v2 (v, 0)
  U v -> v2 (0, negate v)
  D v -> v2 (0, v)

part1 = product . sum . map toV2

toAimCmd = \case
  F v -> (\(V3 (h, d, a)) -> v3 (h + v, d + a * v, a))
  U v -> (+ v3 (0, 0, negate v))
  D v -> (+ v3 (0, 0, v))

part2 = product . dropZ . foldl (flip ($)) 0 . map toAimCmd

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
