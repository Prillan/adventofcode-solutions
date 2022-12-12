{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid
import AoC.Search

-- TODO: Clean up

import Data.Char
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

parse = id

parseAll = parseMapGrid id

-- TODO: Extract as helpers to AoC.Grid
type Pos = (Int, Int)
hv :: Pos -> [Pos]
hv (i, j) = [ (i + dx, j + dy) | dx <- [-1..1]
                               , dy <- [-1..1]
                               , dx == 0 || dy == 0 ]

hvNeighbors m =
  mapMaybe (\pos -> sequence (pos, pos `HashMap.lookup` m))
  . hv

validElev from to =
  to <= from + 1

part1 g =
  let Just (start, _) = find ((== 'S') . snd) . HashMap.toList $ g
      Just (end,   _) = find ((== 'E') . snd) . HashMap.toList $ g
      g' = HashMap.map ord
           . HashMap.insert start 'a'
           . HashMap.insert end 'z'
           $ g

      neighbors (pos, elev) = filter (validElev elev . snd) $ hvNeighbors g' pos
  in fromJust $ bfs_ ((== end) . fst) neighbors (start, ord 'a')

-- TODO: Refactor and implement more efficient searching
part2 g = 
  let Just (start, _) = find ((== 'S') . snd) . HashMap.toList $ g
      Just (end,   _) = find ((== 'E') . snd) . HashMap.toList $ g
      g' = HashMap.map ord
           . HashMap.insert start 'a'
           . HashMap.insert end 'z'
           $ g
      neighbors (pos, elev) = filter (flip validElev elev . snd) $ hvNeighbors g' pos
  in
    fromJust $ bfs_ ((== ord 'a') . snd) neighbors (end, ord 'z')


main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
