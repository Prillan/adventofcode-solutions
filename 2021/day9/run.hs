{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Data.Foldable
import Data.Bits (xor)
import Data.Ord (comparing)
import Data.Bifunctor
import Data.Maybe
import Data.List
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

parseAll = readMapGrid @Int

type Pos = (Int, Int)

neighbors :: Pos -> [Pos]
neighbors (i, j) = [ (i + dx, j + dy) | dx <- [-1..1]
                                      , dy <- [-1..1]
                                      , not (dx == 0 && dy == 0) ]

hv :: Pos -> [Pos]
hv (i, j) = [ (i + dx, j + dy) | dx <- [-1..1]
                               , dy <- [-1..1]
                               , dx == 0 || dy == 0 ]



hvNeighbors :: MapGrid Int -> Pos -> [(Pos, Int)]
hvNeighbors m =
  mapMaybe (\pos -> sequence (pos, pos `HashMap.lookup` m))
  . hv

neighborVals :: MapGrid Int -> Pos -> [Int]
neighborVals m = mapMaybe (`HashMap.lookup` m) . neighbors

lowPoints :: MapGrid Int -> HashMap Pos Int
lowPoints m = HashMap.filterWithKey f m
  where f pos val = all (> val) $ neighborVals m pos

basin :: MapGrid Int -> Pos -> Int -> Set (Pos, Int)
basin m pos val = fixpoint f (Set.singleton (pos, val))
  where f :: Set (Pos, Int) -> Set (Pos, Int)
        f b = Set.union b . Set.unions . Set.map expand $ b

        expand (k, v) =
          Set.fromList
          . filter (validExpand v . snd)
          $ hvNeighbors m k

        validExpand v v' = v < v' && v' < 9

part1 =
  sum
  . map (+1)
  . toList
  . lowPoints

part2 m =
  product
  . take 3
  . sortBy (comparing negate)
  . map length
  . toList
  $ HashMap.mapWithKey (basin m) (lowPoints m)

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
